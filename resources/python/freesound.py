from __future__ import print_function
import freesound
import os
import sys



freesound_client = freesound.FreesoundClient()
freesound_client.set_token('yzAOtKU65NL6aaMaCCaNBk2bypYlLCMwLg9Okzk9')

# Get sound info example
print("Sound info:")
print("-----------")
sound = freesound_client.get_sound(96541)
print("Getting sound:", sound.name)
print("Url:", sound.url)
print("Description:", sound.description)
print("Tags:", " ".join(sound.tags))
print()