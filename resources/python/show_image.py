# show image
# 2019-07-01    PV
import sys
import tkinter as tk
from PIL import Image, ImageTk


# if window is resized, resize the image
def resize_image(event):
    global new_width, new_height
    new_width = event.width
    new_height = event.height
    image = img.resize((new_width, new_height), Image.Resampling.LANCZOS)
    photo = ImageTk.PhotoImage(image)
    label.config(image=photo)
    label.image = photo

root = tk.Tk()
# get Image Name
image_name = image_path.split("/")[-1]
# remove extension
image_name = image_name.split(".")[0]
# get extension
image_extension = image_path.split(".")[-1]

# check if extension is valid
if image_extension not in ["jpg", "jpeg", "png", "gif"]:
    print("Invalid image extension")
    sys.exit(1)

root.title(image_name)
img = Image.open(image_path)
# find the ratio of the new image to the old image
ratio = 600 / img.size[1]
root.geometry("{}x{}".format(int(img.size[0] * ratio), int(img.size[1] * ratio)))
# resize the image based on the calculated ratio
new_width = int(img.size[0] * ratio)
new_height = int(img.size[1] * ratio)

# create a label
label = tk.Label(root)
label.pack(fill=tk.BOTH, expand=tk.YES)
# bind the resize event
root.bind("<Configure>", resize_image)
# set the image
image = img.resize((new_width, new_height), Image.Resampling.LANCZOS)
photo = ImageTk.PhotoImage(image)
label.config(image=photo)
label.image = photo
root.mainloop()





