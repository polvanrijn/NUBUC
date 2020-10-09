# Title     : split_sentences_story_corpus.R
# Objective : Split stories into sentences
# Created by: pol.van-rijn
# Created on: 11.05.20

# Set the right directory
wd = '/Users/pol.van-rijn/MPI/Collaboration/NYU'
setwd(wd)
sentences_df = read.csv('data/stories/original/sentences_df.csv')

# There are 8 stories, so each sentence ID should occur 8 times
if (!all(table(sentences_df$sent_ID) == 8)) {
  stop('Each sentence ID must be given each of all 8 stories')
}

# Read a Excel file that notes all differences between story and recording of the story, i.e. the speaker misread the story
library(readxl)
library(dplyr)
correction_df = read_excel('data/stories/original/disciptancies_between_recording_and_text.xlsx')

replace_characters = function(X, find_chars, replace_chars) {
  if (length(find_chars) != length(replace_chars)) {
    stop('You must specify same number of characters your looking for as you want to replace!')
  }
  if (length(find_chars) < 1) {
    stop('You need to specify at least one character to replace!')
  }
  for (i in 1:length(find_chars)) {
    X = stringr::str_replace_all(X, find_chars[i], replace_chars[i])
  }
  return(X)
}

# Fix the apostophes
find = paste0("\\", c("’"))
replace = c("'")
correction_df$Text = replace_characters(correction_df$Text, find, replace)
correction_df$Voice = replace_characters(correction_df$Voice, find, replace)

# The stories are the same for the male and female speaker
stories_df = rbind(
  sentences_df %>% mutate(gender = 'M'),
  sentences_df %>% mutate(gender = 'F')
)

# In the story text the target words are marked by some special characters, they need to be removed to
# match the transcript to the story
stories_df$sentence_encoded = stories_df$sentence

strip_meta_characters = function(X) {
  library(stringr)
  forbidden_characters = c('~', '[', ']', '{', '}', '<', '>')
  for (forbidden_char in forbidden_characters) {
    X = str_replace_all(X, paste0("\\", forbidden_char), "")
  }
  return(X)
}

stories_df$sentence = apply(stories_df, 1, function(x) strip_meta_characters(x['sentence']))


# Now adapt the written story to what is being said in the recordings:
for (r in 1:nrow(correction_df)) {
  find = correction_df[[r, "Text"]]
  replace = correction_df[[r, "Voice"]]
  story = correction_df[[r, "Story"]]
  gender = correction_df[[r, "Gender"]]
  sent_ID = correction_df[[r, "Sentence"]]
  row_idx = which(stories_df$sent_ID == sent_ID &
                    stories_df$story_nb == story &
                    stories_df$gender == gender)
  # Within a given story for a given speaker the same sentence ID should only be given once
  if (length(row_idx) != 1) {
    stop('Exactly one row must be specified')
  }
  sentence = stories_df[row_idx, "sentence"]

  # If the sentence does not exist, stop
  if (!grepl(find, sentence)) {
    print(find)
    print(sentence)

    sent_split = strsplit(sentence, '')[[1]]
    find_split = strsplit(find, '')[[1]]
    idx_min = min(which(strsplit(sentence, '')[[1]] != strsplit(find, '')[[1]]))
    idxs = (idx_min - 2):(idx_min + 2)
    print(find_split[idxs])
    print(sent_split[idxs])

    stop('Sentence not contained!')
  }
  stories_df[row_idx, "sentence"] = stringr::str_replace(sentence, find, replace)
}

# The sentence 49a is missing in the male recording, so we'll remove it
old_num_rows = nrow(stories_df)
stories_df = na.omit(stories_df)

if (nrow(stories_df) != (old_num_rows - 1)) {
  stop('Only the sentence 49a may be missing!')
}

stories_df$sentence = lapply(stories_df$sentence, function(x) {
  digits_in_text = c(22, 7, 26, "10TH", 5, 4, 1, 19, 3)
  digits_written = c('TWENTY SECOND', 'SEVEN', "TWENTY SIX", "TENTH", "FIVE", "FOURTH", "ONE", "NINETEEN", "THREE")
  return(replace_characters(x, digits_in_text, digits_written))
})

strip_forbidden_characters = function(X) {
  forbidden_characters = c('.', ',', '!', '~', '[', ']', '{', '}', '-', '<', '>', "'", "’", "´", " ")
  for (forbidden_char in forbidden_characters) {
    X = stringr::str_replace_all(X, paste0("\\", forbidden_char), "")
  }
  return(X)
}

library(rPraat)

sentence_duration = NULL
word_duration = NULL
limit_df = NULL
missing_matches = NULL
str_split_chars = ' |-'
for (g in c('M', 'F')) {
  for (s in 1:8) {
    # Grab the correct textgrid
    tg_name = paste0('data/stories/original/textgrid/', g, '/s', s, '_FwP.TextGrid')
    tg = tg.read(tg_name, encoding = 'auto')

    # Index of all labeled, intervalls
    log_idx = !tg$word$label %in% c('[SP]', '[FP]')
    words_label = strip_forbidden_characters(tg$word$label[log_idx])
    label_start = tg$word$t1[log_idx]
    label_end = tg$word$t2[log_idx]

    # Split words in story
    words_story = strip_forbidden_characters(unlist(lapply(filter(stories_df, story_nb == s, gender == g)$sentence, function(x) {
      strsplit(toupper(x), str_split_chars)
    })))

    sent_IDs = unlist(lapply(filter(stories_df, story_nb == s, gender == g)$sent_ID, function(x) x))

    sentences_story = unlist(lapply(filter(stories_df, story_nb == s, gender == g)$sentence, function(x) {
      return(toupper(x))
    }))

    # Get number of words per sentence
    num_word_per_sent = as.numeric(lapply(filter(stories_df, story_nb == s, gender == g)$sentence, function(x) {
      words = strsplit(toupper(x), str_split_chars)[[1]]
      return(length(words))
    }))

    # Total sentences
    num_items = length(num_word_per_sent)

    # Min and max number of words per
    min_length = min(num_word_per_sent)
    max_length = 30

    # Get the first and last two words per sentence
    lookup_df = data.frame(
      words = unlist(lapply(filter(stories_df, story_nb == s, gender == g)$sentence, function(x) {
        words = strsplit(toupper(x), str_split_chars)[[1]]
        idxs = c(1, 2, length(words) - 1, length(words))
        return(words[idxs])
      })),
      position = rep(1:4, num_items),
      sentence = unlist(lapply(1:num_items, function(x) { rep(x, 4) }))
    )

    # Function to lookup a word pair in the story
    lookup_word = function(word_pair) {
      which(unlist(lapply(2:length(words_label), function(i) {
        all(words_label[c(i - 1, i)] == word_pair)
      })))
    }

    # This is the previously used upper_bound
    min_idx = 0
    for (i in 1:num_items) {
      if (i == 11){
        print(i)
      }

      # Indicates that a match was found
      match_found = FALSE
      lower_bound = c()
      upper_bound = c()

      # Matches first two words of sentence
      word_pair_first = strip_forbidden_characters(filter(lookup_df, sentence == i, position %in% 1:2)$words)
      lower_bound = lookup_word(word_pair_first)

      if (length(lower_bound) != 0) {
        # At least one lower bound found

        # Only include lower bounds that we didn't already use
        lower_bound = lower_bound[lower_bound > min_idx]
        lower_bound = head(lower_bound, 1)

        # Matches last two words of sentence
        word_pair_last = strip_forbidden_characters(filter(lookup_df, sentence == i, position %in% 3:4)$words)
        upper_bound = lookup_word(word_pair_last) + 1

        if (length(upper_bound) != 0) {
          # Match found

          # Only include upper bounds that we didn't already use
          upper_bound = upper_bound[upper_bound > min_idx]

          # In the case of multiple upper bounds
          for (u in upper_bound) {
            # e.g. lower bound = c(1, 100), u = 12 => difference = c(-11, 88)
            difference = lower_bound - u
            # Only allow differences between -30 to -8
            idx = difference <= -min_length & difference >= -max_length

            # If this results in a single match
            if (length(lower_bound[idx]) == 1) {
              lower_bound = lower_bound[idx]
              upper_bound = u
              min_idx = upper_bound
              limit_df = rbind(limit_df, data.frame(
                l = lower_bound,
                u = upper_bound,
                sentence = i,
                gender = g,
                story = s
              ))
              match_found = TRUE
              break
            }
          }

          # If no match was found
          if (!match_found & length(lower_bound) == 1) {
            # Exclude all upper bounds below the lower bound
            upper_bound = upper_bound[upper_bound > lower_bound]
            if (length(upper_bound) == 1) {
              min_idx = upper_bound
              limit_df = rbind(limit_df, data.frame(
                l = lower_bound,
                u = upper_bound,
                sentence = i,
                gender = g,
                story = s
              ))
              match_found = TRUE
            }
          }

          if (!match_found & length(upper_bound) == 1) {
            # Exclude all lower bounds above the upper bound
            lower_bound = lower_bound[lower_bound < upper_bound]
            if (length(lower_bound) == 1) {
              min_idx = lower_bound
              limit_df = rbind(limit_df, data.frame(
                l = lower_bound,
                u = upper_bound,
                sentence = i,
                gender = g,
                story = s
              ))
              match_found = TRUE
            }
          }
        } else {
          message('No upper bound')
        }
      } else {
        message('No lower bound')
      }
      # If nothing was found, report it here:
      if (!match_found) {
        print(sentences_story[i])
        print(paste(story, g, i))
        file.edit(tg_name)
        missing_matches = rbind(missing_matches, data.frame(
          sentence = i,
          gender = g,
          story = s,
          lower_bound = paste(lower_bound, collapse = ', '),
          upper_bound = paste(upper_bound, collapse = ', ')
        ))
      } else {
        lower_bound = lower_bound[1]
        upper_bound = upper_bound[1]
        start_time = label_start[lower_bound]
        end_time = label_end[upper_bound]
        sentence_duration = rbind(sentence_duration, data.frame(
          sentence = i,
          sent_ID = sent_IDs[i],
          gender = g,
          story = s,
          start = start_time,
          end = end_time,
          duration = end_time - start_time
        ))

        word_duration = rbind(word_duration, data.frame(
          t1 = label_start[lower_bound:upper_bound] - start_time,
          t2 = label_end[lower_bound:upper_bound] - start_time,
          duration = label_end[lower_bound:upper_bound] - label_start[lower_bound:upper_bound],
          word = words_label[lower_bound:upper_bound],
          sentence = i,
          sent_ID = sent_IDs[i],
          gender = g,
          story = s
        ))

        clean_tg_labels = gsub('[[:punct:]]+', '', paste(words_label[lower_bound:upper_bound], collapse = " "))
        clean_sentence = gsub('[[:punct:]]+', '', sentences_story[i])

        if (clean_tg_labels != clean_sentence) {
          print(drop(attr(adist(clean_sentence, clean_tg_labels, count = TRUE), "counts")))
          print(clean_tg_labels)
          print(clean_sentence)
        }
      }
    }
  }
}

# Do some checks
for (g in c('M', 'F')) {
  for (s in 1:8) {
    recording_df = filter(sentence_duration, gender == g, story == s)
    sound_path = sprintf("data/stories/original/audio/%s/s%d_rs.wav", g, s)
    for (r in 1:nrow(recording_df)) {
      start = recording_df[r, "start"]
      duration = recording_df[r, "duration"]
      id = recording_df[r, "sent_ID"]
      #sentence = stringr::str_pad(recording_df[r, "sentence"], 3, pad = "0")
      filename = sprintf('%s_%d_%s.wav', g, s, id)
      cmd = sprintf('cd %s; sox data/stories/original/audio/%s/s%d_rs.wav data/stories/sentences/%s trim %f %f', wd, g, s, filename, start, duration)
      system(cmd)
    }
  }
}

write.csv(sentence_duration, 'data/stories/original/durations_all_stories_and_speakers.csv', row.names = F)
write.csv(word_duration, 'data/stories/sentences/relative_word_durations.csv', row.names = F)