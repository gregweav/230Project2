import math
import numpy as np
import h5py
import matplotlib.pyplot as plt
import scipy
from PIL import Image
from scipy import ndimage
import tensorflow as tf
from tensorflow.python.framework import ops
np.random.seed(1)
import csv
from keras import layers
from keras.layers import  Input, Dense, Activation, ZeroPadding2D, BatchNormalization, Flatten, Conv2D
from keras.layers import AveragePooling2D, MaxPooling2D, Dropout, GlobalMaxPooling2D, GlobalAveragePooling2D
from keras.models import Model
from keras.preprocessing import image
from keras.utils import layer_utils
from keras.utils.data_utils import get_file
from keras.applications.imagenet_utils import preprocess_input
import pydot
from IPython.display import SVG
from keras.utils.vis_utils import model_to_dot
from keras.utils import plot_model
import keras.backend as K
K.set_image_data_format('channels_last')
import matplotlib.pyplot as plt
from matplotlib.pyplot import imshow
from keras.models import Sequential
from keras.layers import Reshape, Dense, Activation, Flatten, Convolution1D, Dropout
import keras as keras
from IPython.display import SVG
from keras.utils.vis_utils import model_to_dot
#from keras.utils import plot_mode
from sklearn.preprocessing import normalize


trainData = np.genfromtxt('/Users/Greg/Documents/230Project/biggerTrainingSet.csv',delimiter=',')
print(trainData.shape)
trainData = trainData[1:10001, 1:501]

trainDataCh1 = trainData[:, 0:100]
trainDataCh1 = normalize(trainDataCh1, axis=1, norm='l1')
trainDataCh2 = trainData[:, 99:199]
trainDataCh2 = normalize(trainDataCh2, axis = 1, norm = 'l1')
trainDataCh3 = trainData[:, 199:299]
trainDataCh3 = normalize(trainDataCh3, axis = 1, norm = 'l1')
trainDataCh4 = trainData[:, 299:399]
trainDataCh4 = normalize(trainDataCh4, axis = 1, norm = 'l1')
trainDataCh5 = trainData[:, 399:499]
trainDataCh5 = normalize(trainDataCh5, axis = 1, norm = 'l1')

hugeTrainData = np.zeros((10000, 100, 5))
hugeTrainData[:,:,0] = trainDataCh1
hugeTrainData[:,:,1] = trainDataCh2
hugeTrainData[:,:,2] = trainDataCh3
hugeTrainData[:,:,3] = trainDataCh4
hugeTrainData[:,:,4] = trainDataCh5
print(trainData)
trainDataKey = np.zeros((1,10000), dtype = int)
for i in range(0,1999):
        trainDataKey[0,i] = 2
for i in range(1999,3999):
        trainDataKey[0,i] = 0
for i in range(3999, 5999):
        trainDataKey[0,i] =1
for i in range(5999, 7999):
        trainDataKey[0,i] = 4
for i in range(7999,10000):
        trainDataKey[0,i] = 3


testData = np.genfromtxt('/Users/Greg/Documents/230Project/trainingData.csv',delimiter=',')
testData = testData[1:1001:, 1:501]
testDataKey = np.zeros((1,1000), dtype = int)
for i in range(0,199):
        testDataKey[0,i] = 2
for i in range(199,399):
        testDataKey[0,i] = 0
for i in range(399, 599):
        testDataKey[0,i] =1
for i in range(599, 799):
        testDataKey[0,i] = 4
for i in range(799,1000):
        testDataKey[0,i] = 3

testDataCh1 = testData[:, 0:100]
testDataCh1 = normalize(testDataCh1, axis=1, norm='l1')
testDataCh2 = testData[:, 99:199]
testDataCh2 = normalize(testDataCh2, axis=1, norm='l1')
testDataCh3 = testData[:, 199:299]
testDataCh3 = normalize(testDataCh3, axis=1, norm='l1')
testDataCh4 = testData[:, 299:399]
testDataCh4 = normalize(testDataCh4, axis=1, norm='l1')
testDataCh5 = testData[:, 399:499]
testDataCh5 = normalize(testDataCh5, axis=1, norm='l1')

hugeTestData = np.zeros((1000, 100, 5))
hugeTestData[:,:,0] = testDataCh1
hugeTestData[:,:,1] = testDataCh2
hugeTestData[:,:,2] = testDataCh3
hugeTestData[:,:,3] = testDataCh4
hugeTestData[:,:,4] = testDataCh5

def one_hot_matrix(labels, C):
    """
    Creates a matrix where the i-th row corresponds to the ith class number and the jth column
                     corresponds to the jth training example. So if example j had a label i. Then entry (i,j)
                     will be 1.

    Arguments:
    labels -- vector containing the labels
    C -- number of classes, the depth of the one hot dimension

    Returns:
    one_hot -- one hot matrix
    """

    ### START CODE HERE ###

    # Create a tf.constant equal to C (depth), name it 'C'. (approx. 1 line)
    C = tf.constant(C, name='C')

    # Use tf.one_hot, be careful with the axis (approx. 1 line)
    one_hot_matrix = tf.one_hot(indices=labels, depth=C, axis=0)

    # Create the session (approx. 1 line)
    sess = tf.Session()

    # Run the session (approx. 1 line)
    one_hot = sess.run(one_hot_matrix)

    # Close the session (approx. 1 line). See method 1 above.
    sess.close()

    ### END CODE HERE ###

    return one_hot


trainDataKey = one_hot_matrix(trainDataKey, 5)
trainDataKey = np.squeeze(trainDataKey, axis = 1)

testDataKey = one_hot_matrix(testDataKey, 5)
testDataKey = np.squeeze(testDataKey, axis = 1)


def DSignalConv(input_shape):
    """
    Implementation of the HappyModel.
    
    Arguments:
    input_shape -- shape of the images of the dataset

    Returns:
    model -- a Model() instance in Keras
    """
    
    ### START CODE HERE ###
    # Feel free to use the suggested outline in the text above to get started, and run through the whole
    # exercise (including the later portions of this notebook) once. The come back also try out other
    # network architectures as well. 
        # Define the input placeholder as a tensor with shape input_shape. Think of this as your input image!
    X_input = Input(input_shape)

    # Zero-Padding: pads the border of X_input with zeroes
    X = ZeroPadding1D(1)(X_input)

    # CONV -> BN -> RELU Block applied to X
    X = Conv1D(32, 7, stride=1, name='conv0')(X)
    X = BatchNormalization(axis=3, name='bn0')(X)
    X = Activation('relu')(X)

    # MAXPOOL
    X = MaxPooling2D((2, 2), name='max_pool')(X)

    # FLATTEN X (means convert it to a vector) + FULLYCONNECTED
    X = Flatten()(X)
    X = Dense(1, activation='sigmoid', name='fc')(X)

    # Create model. This creates your Keras model instance, you'll use this instance to train/test the model.
    model = Model(inputs=X_input, outputs=X, name='HappyModel')

    return model
    ### END CODE HERE ###
    
    return model


def DConvNetwork():
	model = Sequential()
	model.add(Convolution1D(filters = 5, kernel_size = 1, strides = 1, padding = 'valid', input_shape =(100,5) ))
	model.add(Activation('relu'))
	model.add(Convolution1D(filters = 5, kernel_size = 1, strides = 1, padding = 'valid'))
	model.add(Activation('relu'))
	model.add(Convolution1D(filters = 5, kernel_size = 1, strides = 10, padding = 'valid'))
	model.add(Flatten())
	model.add(Dropout(0.4))
	model.add(Dense(2048,  activation='relu'))
	model.add(Dense(1024, activation='relu'))
	model.add(Dense(5))
	model.add(Activation('softmax'))
	return model

	#model.add(BatchNormalization())
	#model.add(Dense(128), activation = 'relu')
	#model.add(BatchNormalization())
	#model.add(Dense(256), activation = 'relu')
	#model.add(BatchNormalization())
	#model.add(Dense(128), activation = 'relu')
	#model.add(Dense(5))
	#model.add(Activation('softmax'))
	#return model






#print(hugeTrainData.shape)
#print(trainDataKey.shape)

signalClass = DConvNetwork()
plot_model(signalClass, to_file='/Users/Greg/Documents/230Project/model1.png')
signalClass.compile('adam', 'categorical_crossentropy', metrics=['accuracy'])
print(signalClass.summary())
signalClass.fit(hugeTrainData,trainDataKey.T , epochs=40, batch_size=50)


### START CODE HERE ### (1 line)
preds = signalClass.evaluate(hugeTestData, testDataKey.T, batch_size=32, verbose=1, sample_weight=None)
print()
print ("Loss = " + str(preds[0]))
print ("Test Accuracy = " + str(preds[1]))
#print(signalClass.predict(hugeTestData, verbose = 1))
