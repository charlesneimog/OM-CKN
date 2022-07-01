import json
import os

# list as keys in json file

json_file = 'JSONFILE'
with open(json_file) as f:
    data = json.load(f)

data2 = data
all_keys = data.keys()

# RENAME AS KEYS for new json file
new_dict = {}



## HERE HOW YOU WANT TO RENEAME THE KEYS
for key in all_keys:
    value = data[key]
    new_key = os.path.basename(key)
    new_dict[new_key] = value

new_json_file = 'NEWJSONFILE'
# save json file
with open(new_json_file, 'w') as f:
    json.dump(new_dict, f, indent=4)
