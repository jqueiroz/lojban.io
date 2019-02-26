#!/usr/bin/env python3
import sys
import os
import csv
import json
import copy

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

def load_all_gismu():
    all_gismu = []
    with open ("../resources/gismu.txt", "r") as f:
        for line in f.readlines()[1:]:
            gismu = line.strip().split(' ')[0]
            if len(gismu) == 5:
                all_gismu.append(gismu)
    return all_gismu

def load_all_lujvo():
    all_lujvo = []
    with open ("../resources/lujvo2.txt", "r") as f:
        for line in f.readlines()[1:]:
            lujvo = line.strip().split('    ')[0]
            all_lujvo.append(lujvo)
    return all_lujvo

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
    with open("../resources/MyFreq-COMB_without_dots.txt", "r") as f:
        table = {}
        for line in f.readlines():
            frequency, word = line.split(' ')
            table[word.strip()] = int(frequency)
    return table

# TODO: brivla, not just gismu
def run():
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
        words = sentence['content'].split(' ')
        for word in words:
            score += 10000000 / (1 + frequency_table.get(word, 0)**1.5)
        score /= len(words)**0.5
        return score
    def display_interesting_sentences():
        def is_sentence_interesting(sentence):
            interesting_words = ["citka","cizra","cmene","cusku","djica","djuno","gerna","gleki","jimpe","jundi","klaku","klama","lojbo","mutce","nelci","pilno","sipna","tavla","tsani","valsi","xamgu","zgana"]
            for word in sentence['content'].split(' '):
                if word in interesting_words:
                    return True
            return False
        interesting_sentences = sentences_eng
        interesting_sentences = list(filter(is_sentence_interesting, interesting_sentences))
        interesting_sentences.sort(key=compute_sentence_complexity)
        for sentence in interesting_sentences[:100]:
            print("%.3f\t%s" % (compute_sentence_complexity(sentence), sentence['content']))
            print("\t%s" % sentence['translations'][0]['content'])
        print("Interesting sentences: %d" % len(interesting_sentences))
    def display_frequent_words():
        frequent_words = [k for k, v in frequency_table.items() if v >= 100]
        print(frequent_words)
        print("Frequent words: %d" % len(frequent_words))
    def display_top_brivla():
        blacklist = set(["selpa'i"])
        words = frequency_table.items()
        brivla = filter(lambda x: x[0] in gismu or x[0] in lujvo, words)
        brivla = filter(lambda x: x[0] not in blacklist, brivla)
        brivla = list(brivla)
        brivla.sort(key=lambda x: -x[1])
        interesting_brivla = brivla[:20]
        print("Brivla: %d" % len(brivla))
        for w, f in interesting_brivla:
                print("%7d    %s" % (f, w))
        print()
        print(sorted(list(map(lambda x: x[0], interesting_brivla))))
    def build_exercises():
        words = ["citka","cizra","cmene","cusku","djica","djuno","gerna","gleki","jimpe","jundi","klaku","klama","lojbo","mutce","nelci","pilno","sipna","tavla","tsani","valsi","xamgu","zgana"]
        debug = True
        for word in words:
            sentences = filter_by_word(sentences_eng, word)
            sentences.sort(key=compute_sentence_complexity)
            if debug:
                print("%s: %d" % (word, len(sentences)))
            else:
                print("%s:" % word)
            for sentence in sentences[:10]:
                # if debug:
                    # print("    %.3f\t" % compute_sentence_complexity(sentence), end='')
                if debug:
                    print("%01.2f    %s" % (compute_sentence_complexity(sentence), sentence['content']))
                    print("\t\t%s" % sentence['translations'][0]['content'])
                else:
                    print("\t%s" % sentence['content'])
            if debug:
                print()
                print()

    # display_interesting_sentences()
    display_top_brivla()
    # build_exercises()
    # display_frequent_words()

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
    elif sys.argv[1] == 'run':
        run()
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
