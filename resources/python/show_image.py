import vispy

# with vispy, show the image without interactive controls
vispy.use(app='pyqt5')

from vispy import scene
from vispy.io import read_png
from vispy.visuals.transforms import STTransform

# create a canvas with an ImageVisual
canvas = scene.SceneCanvas(keys='interactive', show=True)
view = canvas.central_widget.add_view()
image = read_png(image_path)

# image is readed inverted
image = image[::-1, :, :]

# create the image visual
image = scene.visuals.Image(image, parent=view.scene, method='subdivide')

# set the camera
view.camera = scene.PanZoomCamera(aspect=1)
view.camera.set_range()

# show the canvas
canvas.show()

# run the app
vispy.app.run()






