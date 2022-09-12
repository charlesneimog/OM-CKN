from om_py import to_om, lispify
import vamp

parameters = vamp.get_parameters_of(vamp_key)

if len(parameters) == 0:
    print("No parameters for this plugin")
elif len(parameters) == 1:
    parameters = parameters[0]


list_of_info = []

for parameter in parameters:
    # print organized description
    try:
        identifier = "Identifier = " + parameter['identifier'] + " || "
    except:
        identifier = "Error: No Identifier" + " || "
    try:
        description = "Description = " + parameter['description'] + " || "
    except:
        description = 'Description = Error: No description' + " || "
    try:
        minimum = "Minimum Value = " + str(parameter['minValue']) + " || "
    except:
        minimum = 'Minimum Value = Error: No minimum value' + " || "
    try:
        maximum = "Maximum Value = " + str(parameter['maxValue']) + " || "
    except:
        maximum = 'Maximum Value = Error: No maximum value' + " || "
    try:
        default = "Default Value = " + str(parameter['defaultValue']) + " || "
    except:
        default = 'Error: No Default Value' + " || "
    try:
        step = "Step = " + str(parameter['step']) + " || "
    except: 
        step = 'Error: No Step' + " || "
    try:
        unit = "Unit = " + str(parameter['unit']) 
    except:
        unit = 'Error: No Unit'
    
    list_of_info.append(identifier +  description + minimum + maximum + default + step + unit)

to_om(list_of_info)

    