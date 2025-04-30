

pacman::p_load(reticulate, keras, tensorflow)

# Only need to run first time I think
# install_tensorflow()

reticulate::py_install('transformers', pip = TRUE)
reticulate::py_install('tf-keras', pip = TRUE)
reticulate::py_install('keras-bert', pip = TRUE)

transformer = reticulate::import('transformers')

physical_devices = tf$config$list_physical_devices('GPU')

# Not working, IDK why
# tf$config$experimental$set_memory_growth(physical_devices[[1]],TRUE)

tf$keras$backend$set_floatx('float32')


# get Tokenizer
transformer$RobertaTokenizer$from_pretrained('roberta-base', do_lower_case=TRUE)

# get Model with weights
transformer$TFRobertaModel$from_pretrained('roberta-base')




reticulate::py_config()


tensorflow::tf_version()




pretrained_path = '/Users/MadisonMerzke/Downloads/uncased_L-12_H-768_A-12'
config_path = file.path(pretrained_path, 'bert_config.json')
checkpoint_path = file.path(pretrained_path, 'bert_model.ckpt')
vocab_path = file.path(pretrained_path, 'vocab.txt')



k_bert = import('keras_bert')
token_dict = k_bert$load_vocabulary(vocab_path)
tokenizer = k_bert$Tokenizer(token_dict)


