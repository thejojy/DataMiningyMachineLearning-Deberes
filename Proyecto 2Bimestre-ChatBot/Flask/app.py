#pip install Flask
#pip install transformers
#pip3 install torch torchvision torchaudio --index-url https://download.pytorch.org/whl/cu117

from flask import Flask, request, render_template

# Importa el tokenizador y el modelo de GPT-2
from transformers import GPT2Tokenizer, GPT2LMHeadModel

app = Flask(__name__)

# Carga el tokenizador y el modelo
tokenizer = GPT2Tokenizer.from_pretrained("datificate/gpt2-small-spanish")
model = GPT2LMHeadModel.from_pretrained("chatbot_finetuned")  # Reemplaza con la ubicación de tu modelo entrenado



app = Flask(__name__)

@app.route("/")
def index():
    return render_template("index.html")

@app.route("/send_message", methods=["POST"])
def send_message():
    print("mesage")
    user_message = request.form.get("user_message")  # Obtén el mensaje del usuario desde el formulario HTML
    
    # Utiliza el modelo para generar una respuesta basada en el mensaje del usuario
    input_ids = tokenizer.encode(user_message, return_tensors="pt")
    response_ids = model.generate(input_ids, max_length=100, num_return_sequences=1, no_repeat_ngram_size=2)
    chatbot_response = tokenizer.decode(response_ids[0], skip_special_tokens=True)

    # Devuelve la respuesta del chatbot al frontend
    return chatbot_response

if __name__ == "__main__":
    app.run(debug=True)



