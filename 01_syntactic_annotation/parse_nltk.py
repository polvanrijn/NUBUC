from nltk.parse.corenlp import CoreNLPParser
from nltk import ParentedTree
import os
import re
import glob
import pandas as pd

new_word_df = pd.read_csv('../data/stories/stories_corrected_pauses/new_word_df.csv')
parser = CoreNLPParser() 
def compute_single_trace(tree, trace, add_S = False):
    if tree.parent() == None:
        if add_S:
            trace.append('S')
    else:
      trace.append(tree._label)
      compute_single_trace(tree.parent(), trace)

def compute_total_trace(tree):
    trace = []
    compute_single_trace(tree, trace)
    trace = list(reversed(trace))
    POS = trace.pop()
    trace = '/'.join(trace)
    return(trace, POS)

# df for the results
results = pd.DataFrame()

def process_sentence(word_array, results):
    txt = ' '.join(word_array).replace('\n', '')
    tree = next(parser.raw_parse(txt))
    tree = ParentedTree.convert(tree)
    leaf_values = tree.leaves()

    if len(leaf_values) != len(word_array):
        print('This may not happen')

    token_count = 0
    for token in leaf_values:
        token_count += 1
        leaf_index = leaf_values.index(token)
        tree_location = tree.leaf_treeposition(leaf_index)
        depth = len(tree_location)
        parent = tree[tree_location[0:(depth - 1)]]
        trace, POS_stanf = compute_total_trace(parent)
        df = pd.DataFrame([{
            'trace': trace,
            'POS_stanf': POS_stanf,
            'cd_idx': story,
            'sentence_count': sentence,
            'token_count': token_count,
            'token': token
        }])
        results = results.append(df, ignore_index=True)
    return str(tree), results

def write_str_to_txt(string, path):
    text_file = open(path, "w")
    text_file.write(string)
    text_file.close()

for gender in ['M', 'F']:
    for story in [s + 1 for s in range(8)]:
        story_trees = []
        for sentence in [s + 1 for s in range(133)]:
            filt_df = new_word_df[
                (new_word_df.sentence == sentence) & (new_word_df.gender == gender) & (new_word_df.story == story)
                ]
            word_array = filt_df.word.values
            try:
                tree_str, results = process_sentence(word_array, results)
            except:
                print(word_array)
            story_trees.append(tree_str)
            sent_ID = filt_df.sent_ID.values[0]
            write_str_to_txt(tree_str, "../public/sentences/syntactic_output/%s_%s_%s.txt" % (gender, story, sent_ID))
            print("Finished: %s %s %s" % (gender, story, sentence))
        write_str_to_txt('\n'.join(story_trees), "../public/stories/syntactic_output/%s_%s.txt" % (gender, story))
results.to_csv('../data/stories/stories_corrected_pauses/syntactic_annotation.csv')