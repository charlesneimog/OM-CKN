from pedalboard import load_plugin
plugin = load_plugin("C:/Users/neimog/OneDrive_usp.br/Documents/REAPER Media/Plugins/VST3/dearVR MICRO.vst3")
all_parameters = list(plugin.parameters.keys())
choosed_parameter = all_parameters[5]
print('================ OM-CKN ==============')
print(f'Showing the good values for the {choosed_parameter}.')
parameter = getattr(plugin, choosed_parameter)
goodvalues = parameter.valid_values

mynewlist = []
for item in goodvalues:
    try:
        int_value = int(item)
    except ValueError:
        pass
    else:
        mynewlist.append(item)

first_number = goodvalues[0]
last_number = goodvalues[-1]

if (goodvalues == mynewlist):
    print (f'The good values are one range starting from {first_number} and finished in {last_number}.')
else:
    print (f'The good values are {goodvalues}.')
