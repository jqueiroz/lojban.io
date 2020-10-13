#!/usr/bin/env python3
import sys
import pprint

def increment(dict, key):
    if key in dict:
        dict[key] += 1
    else:
        dict[key] = 1

def extract_user_id(line):
    if 'provider="google"' in line:
        if 'subject="' in line:
            return "google_" + line.split('subject="')[1].split('"')[0]
        elif 'email="' in line:
            return None
        else:
            raise RuntimeError("Unrecognized format for Google in line: %s" % line)
    elif 'provider="mock"' in line:
        return None
    else:
        raise RuntimeError("Unrecognized provider in line: %s" % line)


def compute(file):
    result = {
        'preferences': {},
        'proficiency': {},
    }
    for line in file:
        if line.startswith("DeckPreferences[") or line.startswith("DeckProficiency["):
            user_id = extract_user_id(line)
            if user_id is None:
                continue
            if line.startswith("DeckPreferences["):
                increment(result['preferences'], user_id)
            elif line.startswith("DeckProficiency["):
                increment(result['proficiency'], user_id)
            else:
                raise RuntimeError("Unhandled line: %s" % line) 
    return result


def run(file):
    result = compute(file)
    pprint.pprint(result)


def main():
    # Validate number of arguments
    if len(sys.argv) < 2:
        print("error: incorrect number of arguments")
        return 1
    # Handle commands
    filename = sys.argv[1]
    with open(filename, "r") as f:
        run(f)

if __name__ == '__main__':
    main()
