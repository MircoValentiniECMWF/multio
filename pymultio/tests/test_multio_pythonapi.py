import os
import pytest

import multiopython


default_dict = {
      "allow_world" : True,
      "parent_comm" : 1,
      "client_comm" : [2,3],
      "server_comm" : [4,5]
    }

def test_initialisation():
    multiopython.Multio(**default_dict)

#def test_initialisation_no_config():
#    with pytest.raises(AttributeError):
#        multiopython.Multio()

def test_multio_version():
    assert(multiopython.Multio(**default_dict).__version__() == '2.0.0')

def test_multio_wrong_config_path():
    incorrect_path = "I_AM_NOT_HERE/multio/config/multio-server.yaml"
    with pytest.raises(multiopython.MultioException):
        multiopython.Multio(config_path=incorrect_path, **default_dict)

def test_multio_config_path():
    config_path = "../../tests/multio/config/testPlan.yaml"
    with pytest.raises(multiopython.MultioException):
        multiopython.Multio(config_path=config_path, **default_dict)

def test_multio_open_close_connections():
    multio = multiopython.Multio(**default_dict)

def test_create_metadata():
    multio = multiopython.Multio(**default_dict)
    metadata = {'category' : 'path',
      'new' : 1,
      'new_float' : 1.0,
      'trigger' : 'step',
      'step': 1
    }
    metadata = multio.create_metadata(metadata)

def test_metadata_set_item_syntax():
    multio = multiopython.Multio(**default_dict)
    metadata = multio.create_metadata(None)
    metadata['category'] = 'path'
    metadata['new'] = 1
    metadata['new_float'] = 1.0
    metadata['trigger'] = 'step'
    metadata['step'] = 1

def test_create_wrong_metadata():
    multio = multiopython.Multio(**default_dict)
    metadata = {'category' : 'path',
      'new' : 1,
      'new_float' : 1.0,
      'trigger' : 'step',
      'step': 1,
      'pair' : (1,2)
    }
    with pytest.raises(TypeError):
        multio.write_field([1.0, 2.0, 3.0, 4.0], metadata)

def test_write_field():
    metadata = {'category' : 'path',
      'new' : 1,
      'new_float' : 1.0,
      'trigger' : 'step',
      'step': 1
    }
    with multiopython.Multio(**default_dict) as multio_object:
        multio_object.write_field(metadata, [1.0, 2.0, 3.0, 4.0])
        multio_object.flush(metadata)
        multio_object.notify(metadata)
        assert(os.path.isfile('testWriteOutput.grib')==True)

def test_write_field_use_metadata_object():

    with multiopython.Multio(**default_dict) as multio_object:
        metadata = multio_object.create_metadata(None)
        metadata['category'] = 'path'
        metadata['new'] = 1
        metadata['new_float'] = 1.0
        metadata['trigger'] = 'step'
        metadata['step'] = 1

        multio_object.write_field(metadata, [1.0, 2.0, 3.0, 4.0])
        multio_object.flush(metadata)
        multio_object.notify(metadata)
        assert(os.path.isfile('testWriteOutput.grib')==True)

def test_write_no_metadata():
    multio_object = multiopython.Multio(**default_dict)

    with pytest.raises(AttributeError):
        multio_object.write_field(None, [1.0, 2.0, 3.0, 4.0])

def test_field_accepted():
    metadata = {'category' : 'path',
      'new' : 1,
      'new_float' : 1.0,
      'trigger' : 'step',
      'step': 1
    }
    multio_object = multiopython.Multio(**default_dict)

    multio_object.write_field(metadata, [1.0, 2.0, 3.0, 4.0])
    multio_object.flush(metadata)
    multio_object.notify(metadata)
    assert multio_object.field_accepted(metadata) == False #Unsure if this should be true or false on return need to look at definition in c api

def test_enter_exit_connections():
    metadata = {'category' : 'path',
      'new' : 1,
      'new_float' : 1.0,
      'trigger' : 'step',
      'step': 1
    }
    with multiopython.Multio(**default_dict) as multio_object:

        multio_object.write_field(metadata, [1.0, 2.0, 3.0, 4.0])
        multio_object.flush(metadata)
        multio_object.notify(metadata)
