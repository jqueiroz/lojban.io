#!/usr/bin/env python3
import sys
import os
import csv
import json
import yaml
import copy
import csv

class TatoebaDatabase:
    def __init__(self, directory):
        self.directory = directory
    def load_sentences(self):
        result = []
        with open(os.path.join(self.directory, 'sentences.csv'), 'r') as f:
            reader = csv.reader(f, delimiter='\t')
            for row in reader:
                result.append({
                    'id': row[0],
                    'language': row[1],
                    'content': row[2],
                })
        return result
    def load_links(self):
        result = []
        with open(os.path.join(self.directory, 'links.csv'), 'r') as f:
            reader = csv.reader(f, delimiter='\t')
            for row in reader:
                result.append({
                    'id1': row[0],
                    'id2': row[1],
                })
        return result

def prepare_json():
    # Load original database
    database = TatoebaDatabase('/storage/Databases/Lojban/tatoeba-dumps/2018-03-30')
    sentences = database.load_sentences()
    links = database.load_links()
    # Identify relevant sentences
    lojban_sentences = filter(lambda s: s['language'] == 'jbo', sentences)
    lojban_ids = set(map(lambda s: s['id'], lojban_sentences))
    translation_ids = set([link['id1'] for link in links if link['id2'] in lojban_ids])
    relevant_ids = lojban_ids | translation_ids
    relevant_sentences = list(filter(lambda s: s['id'] in relevant_ids, sentences))
    relevant_ids = set(map(lambda s: s['id'], relevant_sentences))
    relevant_links = list(filter(lambda l: l['id1'] in relevant_ids and l['id2'] in relevant_ids, links))
    # Save output
    data = {
        'sentences': relevant_sentences,
        'links': relevant_links,
    }
    with open('/tmp/tatoeba-lojban.json', 'w') as f:
        json.dump(data, f, sort_keys=True, indent=4)

def load_sentences_from_json():
    def retrieve_normalized_sentences(data):
        sentences = data['sentences'][:]
        links = data['links']
        sentences_by_id = {}
        for sentence in sentences:
            sentences_by_id[sentence['id']] = sentence
            if sentence['language'] == 'jbo':
                sentence['translations'] = []
        for link in links:
            sentence1 = sentences_by_id[link['id1']]
            sentence2 = sentences_by_id[link['id2']]
            if sentence1['language'] == 'jbo' and sentence2['language'] == 'eng':
                sentence1['translations'].append({
                    'language': sentence2['language'],
                    'content': sentence2['content'],
                })
        for sentence in sentences:
            if sentence['language'] == 'jbo':
                del sentence['language']
        return list(filter(lambda s: 'language' not in s, sentences))

    with open('/storage/Databases/Lojban/tatoeba-dumps/2018-03-30/tatoeba-lojban.json', 'r') as f:
        return retrieve_normalized_sentences(json.load(f))

def load_sentences_from_csv():
    with open('/storage/Databases/Lojban/tatoeba-refined/2017.csv') as csvfile:
        reader = csv.reader(csvfile, delimiter=',')
        sentences = []
        first = True
        for row in reader:
            if first:
                first = False
                continue
            # print(row)
            sentence_in_english = row[1]
            original_sentence_in_lojban = row[2]
            ilmen_tags = row[3]
            ilmen_alternative_proposal = row[4]
            gleki_tags = row[5]
            gleki_alternative_proposal = row[6]
            sentence_in_lojban = None
            if 'G' in ilmen_tags or 'A' in ilmen_tags or 'G' in gleki_tags or 'A' in gleki_tags:
                sentence_in_lojban = original_sentence_in_lojban
            if ilmen_alternative_proposal:
                sentence_in_lojban = ilmen_alternative_proposal
            elif gleki_alternative_proposal:
                sentence_in_lojban = gleki_alternative_proposal
            ilmen_tags = row[3]
            if sentence_in_lojban is None:
                continue
            sentence_in_lojban = sentence_in_lojban.replace(".i ", "")
            sentence = {
                'content': sentence_in_lojban,
                'translations': [
                    {'content': sentence_in_english, 'language': 'eng'}
                ]
            }
            sentences.append(sentence)
            # print("\t", (sentence_in_english, sentence_in_lojban, ilmen_tags, ilmen_alternative_proposal, gleki_tags, gleki_alternative_proposal))
            # print("\t", sentence)
        return sentences

def load_all_gismu():
    all_gismu = []
    with open ("../../resources/language/dictionary-generation/english/gismu.txt", "r") as f:
        for line in f.readlines()[1:]:
            gismu = line[1:6]
            if len(gismu.strip()) == 5:
                all_gismu.append(gismu)
    return all_gismu

def load_all_lujvo():
    all_lujvo = []
    with open ("../../resources/language/dictionary-generation/english/lujvo2.txt", "r") as f:
        for line in f.readlines()[1:]:
            lujvo = line.strip().split('    ')[0]
            all_lujvo.append(lujvo)
    return all_lujvo

def load_brivla_from_yaml_file(filename):
    ret = []
    with open(filename, "r") as f:
        data = yaml.load(f, Loader=yaml.CLoader)
        for brivla, translations in data.items():
            ret.append(brivla)
    return ret

yaml_brivla_files = \
    [ "../../resources/decks/english/brivla/sentences/01.yaml"
    , "../../resources/decks/english/brivla/sentences/02.yaml"
    , "../../resources/decks/english/brivla/sentences/03.yaml"
    , "../../resources/decks/english/brivla/sentences/04.yaml"
    , "../../resources/decks/english/brivla/sentences/05.yaml"
    ]

def load_taught_brivla():
    ret = []
    for filename in yaml_brivla_files:
        ret += load_brivla_from_yaml_file(filename)
    return ret

def print_json(data):
    print(json.dumps(data, sort_keys=True, indent=4))

def filter_by_language(sentences, lang):
    result = []
    for sentence in sentences:
        new_sentence = copy.deepcopy(sentence)
        new_sentence['translations'] = list(filter(lambda s: s['language'] == lang, new_sentence['translations']))
        if new_sentence['translations']:
            result.append(new_sentence)
    return result

def filter_by_word(sentences, word):
    return list(filter(lambda s: word in s['content'].split(' '), sentences))

# builds frequency table from tatoeba
def build_frequency_table(sentences):
    table = {}
    for sentence in sentences:
        for word in sentence['content'].split(' '):
            table[word] = table.get(word, 0) + 1
    return table

# loads frequency table from https://mw.lojban.org/papri/File:MyFreq-COMB_without_dots.txt
def load_frequency_table():
    with open("../../resources/language/frequency-lists/MyFreq-COMB_without_dots.txt", "r") as f:
        table = {}
        for line in f.readlines():
            frequency, word = line.split(' ')
            table[word.strip()] = int(frequency)
    return table

def encode_text_to_yaml_string(text):
    if "#" in text:
        return "\"%s\"" % text
    else:
        return text

# TODO: brivla, not just gismu
def run(cmd):
    sentences_eng = filter_by_language(load_sentences_from_json(), 'eng')
    print("Sentences: %d" % len(sentences_eng))
    frequency_table = load_frequency_table()
    print("Words: %d" % len(frequency_table))
    gismu = set(load_all_gismu())
    print("Gismu: %d" % len(gismu))
    lujvo = set(load_all_lujvo())
    print("Lujvo: %d" % len(lujvo))
    print("-----------")
    print()
    def compute_sentence_complexity(sentence):
        score = 0
        words = sentence.split(' ')
        for word in words:
            word = word.replace(".", "")
            score += 10000000 / (1 + frequency_table.get(word, 0)**1.5)
        score /= len(words)**0.5
        return score
    def display_frequent_words():
        frequent_words = [k for k, v in frequency_table.items() if v >= 100]
        print(frequent_words)
        print("Frequent words: %d" % len(frequent_words))
    def retrieve_top_brivla():
        blacklist = set(["selpa'i", "broda", "gerna", "lojbo", "tsani", "zmadu", "gerna", "binxo", "terdatni", "srana", "binxo", "casnu", "jbopre", "cmavo", "lujvo", "gismu", "nuzba", "dukse", "ninmu"])
        taught = set(load_taught_brivla())
        words = frequency_table.items()
        brivla = filter(lambda x: x[0] in gismu or x[0] in lujvo, words)
        brivla = filter(lambda x: x[0] not in blacklist, brivla)
        brivla = filter(lambda x: x[0] not in taught, brivla)
        brivla = list(brivla)
        brivla.sort(key=lambda x: -x[1])
        return brivla
    def display_top_brivla():
        brivla = retrieve_top_brivla()
        interesting_brivla = brivla[:20]
        print("Brivla: %d" % len(brivla))
        for w, f in interesting_brivla:
                print("%7d    %s" % (f, w))
        print()
        print(list(map(lambda x: x[0], interesting_brivla)))
        # print(sorted(list(map(lambda x: x[0], interesting_brivla))))
    def display_top_brivla_places():
        brivla = retrieve_top_brivla()
        interesting_brivla = brivla[:20]
        print("Brivla: %d" % len(brivla))
        print()
        for w, f in interesting_brivla:
            print("%s:" % w)
            for x in range(1, 6):
                print("    x%d: " % x)
            print()
        print()
        print(list(map(lambda x: x[0], interesting_brivla)))
    def enrich_yaml(raw_data):
        lines = []
        data = yaml.load(raw_data, Loader=yaml.CLoader)
        for brivla , translations in data.items():
            lines.append("# Total sentences: %d" % len(filter_by_word(sentences_eng, brivla)))
            lines.append("%s:" % brivla)
            for translation in translations:
                lines.append("    # Sentence complexity: %.3f" % compute_sentence_complexity(translation['lojban_sentences'][0]))
                lines.append("    - lojban_sentences:")
                for lojban_sentence in translation['lojban_sentences']:
                    lines.append("        - %s" % encode_text_to_yaml_string(lojban_sentence))
                lines.append("      translated_sentences:")
                for translated_sentence in translation['translated_sentences']:
                    lines.append("        - %s" % encode_text_to_yaml_string(translated_sentence))
                lines.append("")
            lines.append("")
            lines.append("")
        return '\n'.join(lines)
    def enrich_exercises():
        for filename in yaml_brivla_files:
            with open(filename, "r+") as f:
                raw_data = f.read()
                enriched_data = enrich_yaml(raw_data)
                f.seek(0)
                f.truncate(0)
                f.write(enriched_data)
    def build_exercises():
        # prenu, tsani, zmadu
        # words = ['tsali', 'viska', 'casnu', 'jinvi', 'jbopre', 'tadni', 'ponse', 'bebna', 'pluka', 'nandu', 'preti', 'prami', 'tatpi', 'fonxa', 'morji', 'certu', 'xabju', 'ckule', 'facki', 'srera']
        brivla = retrieve_top_brivla()[:20]
        words = list(map(lambda x: x[0], brivla))
        for word in words:
            sentences = filter_by_word(sentences_eng, word)
            sentences.sort(key=lambda s: compute_sentence_complexity(s['content']))
            print("# Total sentences: %d" % len(sentences))
            print("%s:" % word)
            for sentence in sentences[:10]:
                print("    # Sentence complexity: %.3f" % compute_sentence_complexity(sentence['content']))
                print("    - lojban_sentences:")
                print("        - %s" % encode_text_to_yaml_string(sentence['content']))
                print("      translated_sentences:")
                print("        - %s" % encode_text_to_yaml_string(sentence['translations'][0]['content']))
                print()
            print()
            print()

    if cmd == 'top':
        display_top_brivla()
    if cmd == 'top-places':
        display_top_brivla_places()
    elif cmd == 'exercises':
        build_exercises()
    elif cmd == 'enrich':
        enrich_exercises()
    elif cmd == 'interesting_sentences':
        display_interesting_sentences()
    elif cmd == 'frequent_words':
        display_frequent_words()

def search(word):
    sentences_eng = filter_by_language(load_sentences(), 'eng')
    print_json(filter_by_word(sentences_eng, word))

def main():
    # Validate number of arguments
    if len(sys.argv) < 2:
        print("error: incorrect number of arguments")
        return 1
    # Handle commands
    if sys.argv[1] == 'prepare':
        prepare_json()
    elif sys.argv[1] == 'top':
        run('top')
    elif sys.argv[1] == 'top-places':
        run('top-places')
    elif sys.argv[1] == 'exercises':
        run('exercises')
    elif sys.argv[1] == 'enrich':
        run('enrich')
    elif sys.argv[1] == 'search':
        if len(sys.argv) != 3:
            print("error: incorrect number of arguments")
            return 1
        search(sys.argv[2])
    else:
        print("error: incorrect unrecognized command")
        return 1

if __name__ == '__main__':
    main()
