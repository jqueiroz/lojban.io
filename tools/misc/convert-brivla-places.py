#!/usr/bin/env python3

all_brivla = [ ("tavla", ["speaker", "listener", "subject", "language"])
    , ("dunda", ["donor", "gift", "recipient"])
    , ("ctuca", ["instructor", "audience/student(s)", "ideas/methods", "subject", "teaching method"])
    , ("citka", ["consumer", "aliment"])
    , ("ciska", ["writer", "text/symbols", "display/storage medium", "writing implement"])
    , ("klama", ["traveler", "destination", "origin", "route", "means/vehicle"])
    , ("bridi", ["predicate relationship", "relation", "arguments"])
    , ("djuno", ["knower", "facts", "subject", "epistemology"])
    , ("nupre", ["promisor", "promise", "beneficiary/victim"])
    , ("cusku", ["expresser", "message", "audience", "expressive medium"])
    , ("cizra", ["strange thing", "viewpoint holder", "property"])
    , ("cmene", ["name/title", "name posessor", "name-giver/name-user"])
    , ("cusku", ["agent", "expressed idea", "audience", "expressive medium"])
    , ("djica", ["desirer", "event/state", "purpose"])
    , ("gleki", ["happy entity", "event/state"])
    , ("jimpe", ["understander", "fact/truth", "subject"])
    , ("klama", ["traveler", "destination", "origin", "route", "means/vehicle"])
    , ("mutce", ["much/extreme thing", "property", "extreme/direction"])
    , ("nelci", ["liker", "object/state"])
    , ("pilno", ["user", "instrument", "purpose"])
    , ("sipna", ["asleep entity"])
    , ("xamgu", ["good object/event", "beneficiary", "standard"])
    , ("zgana", ["observer", "observed", "senses/means", "conditions"])
    , ("bangu", ["language/dialect", "language user", "communicated idea"])
    , ("cliva", ["agent", "point of departure", "route"])
    , ("finti", ["inventor/composer", "invention", "purpose", "existing elements/ideas"])
    , ("gunka", ["worker", "activity", "goal"])
    , ("jundi", ["attentive entity", "object/affair"])
    , ("kakne", ["capable entity", "capability", "conditions"])
    , ("tcidu", ["reader", "text", "reading material"])
    , ("valsi", ["word", "meaning", "language"])
    , ("zvati", ["atendee/event", "location"])
    , ("cinri", ["interesting abstraction", "interested entity"])
    , ("drata", ["entity #1", "entity #2", "standard"])
    , ("simsa", ["entity #1", "entity #2", "property/quantity"])
    , ("klaku", ["crier", "tears", "reason"])
    , ("melbi", ["beautiful entity", "viewpoint holder", "aspect", "aesthetic standard"])
    , ("smuni", ["meaning/interpretation", "expression", "opinion holder"])
    , ("vecnu", ["seller", "goods/service", "buyer", "price"])
    , ("plise", ["apple", "species/strain"])
    , ("prenu", ["person"])
    , ("cilre", ["learner", "facts", "subject", "source", "method"])
    , ("cnino", ["new entity", "observer", "feature", "standard"])
    , ("drani", ["correct thing", "property", "situation", "standard"])
    , ("fanva", ["translator", "text/utterance", "target language", "source language", "translation result"])
    , ("gasnu", ["agent", "event"])
    , ("kelci", ["player", "toy"])
    , ("milxe", ["mild thing", "property"])
    , ("mlatu", ["cat", "species/breed"])
    , ("nitcu", ["needing entity", "necessity", "purpose"])
    , ("pendo", ["friendly entity", "friendliness experiencer"])
    , ("pensi", ["thinking entity", "subject/concept"])
    , ("skami", ["computer", "purpose"])
    , ("slabu", ["familiar/old thing", "observer", "feature", "standard"])
    , ("troci", ["trier", "attempted event/state/property", "actions/method"])
    , ("zdani", ["house", "owner/user"])
    ]

def encode_text(text):
    if "#" in text:
        return "\"%s\"" % text
    else:
        return text

for brivla in all_brivla:
    word = brivla[0]
    places = brivla[1]
    x1, x2, x3, x4, x5 = [None, None, None, None, None]
    try:
        x1 = encode_text(places[0])
        x2 = encode_text(places[1])
        x3 = encode_text(places[2])
        x4 = encode_text(places[3])
        x5 = encode_text(places[4])
    except:
        pass
    print("%s:" % word)
    if x1:
        print("    x1: %s" % x1)
    if x2:
        print("    x2: %s" % x2)
    if x3:
        print("    x3: %s" % x3)
    if x4:
        print("    x4: %s" % x4)
    if x5:
        print("    x5: %s" % x5)
    print()
