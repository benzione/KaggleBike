# %%
import tensorflow as tf
from tensorflow.keras.layers import Embedding, LSTM, Dense, Dropout
from tensorflow.keras.preprocessing.text import Tokenizer
from tensorflow.keras.preprocessing.sequence import pad_sequences
from sklearn.preprocessing import OneHotEncoder
from sklearn.model_selection import train_test_split
from scipy.sparse import csr_matrix
import numpy as np
import pandas as pd

tf.random.set_seed(7)


# %%
source_df = pd.read_csv('Hydra-Movie-Scrape.csv')

# %%
df = source_df[['Summary',  'Genres']]

# %%
df['label'] = df['Genres'].apply(lambda x: x.split('|')[0])
df = df.astype(str)

# %%
tokenizer = Tokenizer()
tokenizer.fit_on_texts(df['Summary'])
sequences = tokenizer.texts_to_sequences(df['Summary'])
max_length = max([len(s) for s in sequences])
sequences = pad_sequences(sequences, maxlen=max_length)

# %%
encoder = OneHotEncoder()
labels = np.array(df['label'])
y = encoder.fit_transform(labels.reshape(-1, 1))

# %%
y = y.toarray()
x_train, x_test, y_train, y_test = train_test_split(sequences, y, test_size=0.2, shuffle=True, random_state=42)

# %%
model = tf.keras.Sequential()
vocab_size = len(tokenizer.word_index) + 1
embedding_dim = 150
model.add(Embedding(input_dim=vocab_size, output_dim=embedding_dim, input_length=max_length))
model.add(Dropout(0.2))
model.add(LSTM(100))
model.add(Dropout(0.2))

# %%
model.add(Dense(units=df['label'].nunique(), activation='sigmoid'))
model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])

# %%
model.fit(x_train, y_train, epochs=10, batch_size=32)

# %%
test_loss, test_acc = model.evaluate(x_test, y_test)
print('Test Loss: {}'.format(test_loss))
print('Test Accuracy: {}'.format(test_acc))

# %%
from transformers import MarianMTModel, MarianTokenizer

# Load the pre-trained MarianMT model and tokenizer for Hebrew to English translation
model_name = 'C:\\Users\\shaam\\Documents\\movie\\opus-mt-tc-big-he-en'
tokenizer = MarianTokenizer.from_pretrained(model_name)
model = MarianMTModel.from_pretrained(model_name)

# Define the text to translate
text = "תן לי לעזור לעזור לך"

# Tokenize and translate
translated = model.generate(**tokenizer(text, return_tensors="pt", padding=True))

# Decode the translation
translated_text = tokenizer.batch_decode(translated, skip_special_tokens=True)[0]

print(translated_text)

# %%
from transformers import pipeline
pipe = pipeline("translation", model="C:\\Users\\shaam\\Documents\\movie\\opus-mt-tc-big-he-en")
print(pipe("היא שכחה לכתוב לו."))

# %%



