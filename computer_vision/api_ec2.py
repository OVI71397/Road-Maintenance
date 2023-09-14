import tensorflow as tf
from tensorflow.keras.models import load_model
import numpy as np
from keras.preprocessing.image import load_img, img_to_array 
from flask import Flask, jsonify, request

model = load_model('models/MobNet_model_3classes.h5')

def predict_defect(image_path):
    img = load_img(image_path, target_size= (224, 224, 3))
    img = img_to_array(img)
    yhat = model.predict(np.expand_dims(img/255, 0)).argmax(axis=1)
    if yhat == 0:
        pred = 'crack'
    elif yhat == 1:
        pred = 'pothole'
    else:
        pred = 'rut'
    return pred
  
app = Flask(__name__)

@app.route('/', methods=['POST'])

def infer_image():
    if 'file' not in request.files:
        return jsonify(error="Please try again. The image doesn't exist")
    
    file = request.files.get('file')
    img_bytes = file.read()
    img_path = "./images/crack3.jpg"
    with open(img_path, "wb") as img:
        img.write(img_bytes)
    result = predict_defect(img_path)
    return jsonify(prediction=result)
  
if __name__ == '__main__':
    app.run(debug=False)
