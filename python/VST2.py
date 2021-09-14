from cython_vst_loader.vst_host import VstHost
from cython_vst_loader.vst_plugin import VstPlugin
from cython_vst_loader.vst_loader_wrapper import allocate_float_buffer


sample_rate = 44100
buffer_size = 512

host = VstHost(sample_rate, buffer_size)
right_output = allocate_float_buffer(buffer_size, 1)
left_output = allocate_float_buffer(buffer_size, 1)

plugin_path = "C:/Users/neimog/OneDrive - design.ufjf.br/Documentos/REAPER Media/Plugins/MConvolutionEZ.dll"
plugin = VstPlugin(plugin_path.encode('utf-8'), host)

assert (b'C:/Users/neimog/OneDrive - design.ufjf.br/Documentos/REAPER Media/M4L-ConvolutionReverb/02 Ambience and Small Rooms/BM7 Small Room.aif' == plugin.get_parameter_name(4).decode('utf-8'))
