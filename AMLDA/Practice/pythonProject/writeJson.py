import json

dictionary = {
    "ID": 2,
    "Name": "samir",
    "Surname": "aliyev",
    "Phone": 123456,
    "eMail": "samir@ada.edu.az"
}

jsonObj = json.dumps(dictionary, indent=4)

print(jsonObj)

with open("data.json", "w") as file:
    file.write(jsonObj)
