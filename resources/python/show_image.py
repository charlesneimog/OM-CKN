# show image
# 2019-07-01    PV
import sys
import tkinter as tk
from PIL import Image, ImageTk



root = tk.Tk()

# get Image Name
image_name = image_path.split("/")[-1]

# remove extension
image_name = image_name.split(".")[0]

root.title(image_name)


img = Image.open(image_path)
# find the ratio of the new image to the old image
ratio = 600 / img.size[1]

root.geometry("{}x{}".format(int(img.size[0] * ratio), int(img.size[1] * ratio)))
# resize the image based on the calculated ratio
new_width = int(img.size[0] * ratio)
new_height = int(img.size[1] * ratio)

# DeprecationWarning: ANTIALIAS is deprecated and will be removed in Pillow 10 (2023-07-01). Use Resampling.LANCZOS instead.

img = img.resize((new_width, new_height), Image.Resampling.LANCZOS)
img = ImageTk.PhotoImage(img)

panel = tk.Label(root, image=img)
panel.pack(side="bottom", fill="both", expand="yes")

root.mainloop()




