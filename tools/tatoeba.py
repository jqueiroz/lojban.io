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

def prepare():
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

def normalize(data):
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

def filter_by_language(sentences, lang):
    result = []
    for sentence in sentences:
        new_sentence = copy.deepcopy(sentence)
        new_sentence['translations'] = list(filter(lambda s: s['language'] == lang, new_sentence['translations']))
        if new_sentence['translations']:
            result.append(new_sentence)
    return result

def print_json(data):
    print(json.dumps(data, sort_keys=True, indent=4))

def filter_by_word(sentences, word):
    return list(filter(lambda s: word in s['content'].split(' '), sentences))

def load_sentences():
    with open('//storage/Databases/Lojban/tatoeba-dumps/2018-03-30/tatoeba-lojban.json', 'r') as f:
        return normalize(json.load(f))

def run():
    sentences_eng = filter_by_language(load_sentences(), 'eng')
    x = filter_by_word(sentences_eng, 'nupre')
    print_json(x)

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
        prepare()
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
