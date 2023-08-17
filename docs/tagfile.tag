<?xml version='1.0' encoding='UTF-8' standalone='yes' ?>
<tagfile doxygen_version="1.9.6">
  <compound kind="file">
    <name>multio_fapi.F90</name>
    <path>/etc/ecmwf/nfs/dh1_perm_a/mavm/projects/multio-bundle/ifs-bundle/source/multio/src/multio/api/fortran/</path>
    <filename>multio__fapi_8F90.html</filename>
    <namespace>multio_api</namespace>
  </compound>
  <compound kind="file">
    <name>multio_fapi_base_handle.F90</name>
    <path>/etc/ecmwf/nfs/dh1_perm_a/mavm/projects/multio-bundle/ifs-bundle/source/multio/src/multio/api/fortran/</path>
    <filename>multio__fapi__base__handle_8F90.html</filename>
    <class kind="type">multio_api_base_handle_mod::multio_base_handle</class>
    <namespace>multio_api_base_handle_mod</namespace>
    <member kind="function">
      <type>type(c_ptr) function</type>
      <name>multio_base_handle_c_ptr</name>
      <anchorfile>namespacemultio__api__base__handle__mod_afbf109b8fff00ce6024bd4ec18eed64d.html</anchorfile>
      <anchor>afbf109b8fff00ce6024bd4ec18eed64d</anchor>
      <arglist>(handle)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_base_handle_new</name>
      <anchorfile>namespacemultio__api__base__handle__mod_a303bc6624a0fd1d4812bb2f4c5b154cf.html</anchorfile>
      <anchor>a303bc6624a0fd1d4812bb2f4c5b154cf</anchor>
      <arglist>(handle, cc)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_base_handle_new_default</name>
      <anchorfile>namespacemultio__api__base__handle__mod_aeeb92733e88a8194bae18279ce600062.html</anchorfile>
      <anchor>aeeb92733e88a8194bae18279ce600062</anchor>
      <arglist>(handle)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_base_handle_delete</name>
      <anchorfile>namespacemultio__api__base__handle__mod_aa8208cf8ac30c02b14fe6bd3c3ab233a.html</anchorfile>
      <anchor>aa8208cf8ac30c02b14fe6bd3c3ab233a</anchor>
      <arglist>(handle)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_base_handle_set_failure_handler</name>
      <anchorfile>namespacemultio__api__base__handle__mod_a6fc73e6606f0f89e0ee9391974153633.html</anchorfile>
      <anchor>a6fc73e6606f0f89e0ee9391974153633</anchor>
      <arglist>(handle, handler, context)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_base_handle_open_connections</name>
      <anchorfile>namespacemultio__api__base__handle__mod_a9d7aa8a221614ef76a9cacf4ecb07307.html</anchorfile>
      <anchor>a9d7aa8a221614ef76a9cacf4ecb07307</anchor>
      <arglist>(handle)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_base_handle_close_connections</name>
      <anchorfile>namespacemultio__api__base__handle__mod_ad745a00ea651391354b017e56293f6ed.html</anchorfile>
      <anchor>ad745a00ea651391354b017e56293f6ed</anchor>
      <arglist>(handle)</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>multio_fapi_configuration.F90</name>
    <path>/etc/ecmwf/nfs/dh1_perm_a/mavm/projects/multio-bundle/ifs-bundle/source/multio/src/multio/api/fortran/</path>
    <filename>multio__fapi__configuration_8F90.html</filename>
    <class kind="type">multio_api_configuration_mod::multio_configuration</class>
    <namespace>multio_api_configuration_mod</namespace>
    <member kind="function">
      <type>integer(c_int) function, pointer</type>
      <name>multio_conf_move_failure_id</name>
      <anchorfile>namespacemultio__api__configuration__mod_adb38eec6d1528ae41fade7924b1a2fa1.html</anchorfile>
      <anchor>adb38eec6d1528ae41fade7924b1a2fa1</anchor>
      <arglist>(cc)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>type(c_ptr) function</type>
      <name>multio_configuration_c_ptr</name>
      <anchorfile>namespacemultio__api__configuration__mod_a2972d422d8530f2b2ff40ae128fe3f7f.html</anchorfile>
      <anchor>a2972d422d8530f2b2ff40ae128fe3f7f</anchor>
      <arglist>(cc)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_new_configuration</name>
      <anchorfile>namespacemultio__api__configuration__mod_a6cab6484fd7472c2d1e3a04d6b7e48be.html</anchorfile>
      <anchor>a6cab6484fd7472c2d1e3a04d6b7e48be</anchor>
      <arglist>(cc)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_new_configuration_from_filename</name>
      <anchorfile>namespacemultio__api__configuration__mod_a44c186e3ed8bde6441170e25a19b4697.html</anchorfile>
      <anchor>a44c186e3ed8bde6441170e25a19b4697</anchor>
      <arglist>(cc, file_name)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_delete_configuration</name>
      <anchorfile>namespacemultio__api__configuration__mod_a5dea51f6e0dc91d263697b7689a490fc.html</anchorfile>
      <anchor>a5dea51f6e0dc91d263697b7689a490fc</anchor>
      <arglist>(cc)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_conf_set_failure_handler</name>
      <anchorfile>namespacemultio__api__configuration__mod_a6f3af8ffec1aa7b890b818fb721dd827.html</anchorfile>
      <anchor>a6f3af8ffec1aa7b890b818fb721dd827</anchor>
      <arglist>(cc, handler, context)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_conf_set_path</name>
      <anchorfile>namespacemultio__api__configuration__mod_a7df080b003fdccccf771461b3583946a.html</anchorfile>
      <anchor>a7df080b003fdccccf771461b3583946a</anchor>
      <arglist>(cc, path)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_conf_mpi_allow_world_default_comm</name>
      <anchorfile>namespacemultio__api__configuration__mod_a970555b9abe60ae30498de1232a33fb3.html</anchorfile>
      <anchor>a970555b9abe60ae30498de1232a33fb3</anchor>
      <arglist>(cc, allow)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_conf_mpi_parent_comm</name>
      <anchorfile>namespacemultio__api__configuration__mod_ae28d4759a7ed6ad5672d08d6837842fa.html</anchorfile>
      <anchor>ae28d4759a7ed6ad5672d08d6837842fa</anchor>
      <arglist>(cc, parent_comm)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_conf_mpi_return_client_comm</name>
      <anchorfile>namespacemultio__api__configuration__mod_abaea42ca5b83c26835a5a809431957f2.html</anchorfile>
      <anchor>abaea42ca5b83c26835a5a809431957f2</anchor>
      <arglist>(cc, return_comm)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_conf_mpi_return_server_comm</name>
      <anchorfile>namespacemultio__api__configuration__mod_a2803989846b5fef66f23a0bb8cbee9c0.html</anchorfile>
      <anchor>a2803989846b5fef66f23a0bb8cbee9c0</anchor>
      <arglist>(cc, return_comm)</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>multio_fapi_constants.F90</name>
    <path>/etc/ecmwf/nfs/dh1_perm_a/mavm/projects/multio-bundle/ifs-bundle/source/multio/src/multio/api/fortran/</path>
    <filename>multio__fapi__constants_8F90.html</filename>
    <namespace>multio_api_constants_mod</namespace>
    <member kind="variable">
      <type>integer, parameter, public</type>
      <name>multio_success</name>
      <anchorfile>namespacemultio__api__constants__mod_adc8dc0bff9f499822da99761fe76d03e.html</anchorfile>
      <anchor>adc8dc0bff9f499822da99761fe76d03e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter, public</type>
      <name>multio_error_eckit_exception</name>
      <anchorfile>namespacemultio__api__constants__mod_a485e4a9ab559ed2d5fa4ae901bf58734.html</anchorfile>
      <anchor>a485e4a9ab559ed2d5fa4ae901bf58734</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter, public</type>
      <name>multio_error_general_exception</name>
      <anchorfile>namespacemultio__api__constants__mod_aa67cb372ced6bff634f05fa5a411d92b.html</anchorfile>
      <anchor>aa67cb372ced6bff634f05fa5a411d92b</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter, public</type>
      <name>multio_error_unknown_exception</name>
      <anchorfile>namespacemultio__api__constants__mod_a49efea9795a2a133e67b6cb35e2f1f4e.html</anchorfile>
      <anchor>a49efea9795a2a133e67b6cb35e2f1f4e</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>multio_fapi_data.F90</name>
    <path>/etc/ecmwf/nfs/dh1_perm_a/mavm/projects/multio-bundle/ifs-bundle/source/multio/src/multio/api/fortran/</path>
    <filename>multio__fapi__data_8F90.html</filename>
    <class kind="type">multio_api_data_mod::multio_data</class>
    <namespace>multio_api_data_mod</namespace>
    <member kind="function">
      <type>type(c_ptr) function</type>
      <name>multio_data_c_ptr</name>
      <anchorfile>namespacemultio__api__data__mod_a389eca5e3ea0e20693b4faa4760167be.html</anchorfile>
      <anchor>a389eca5e3ea0e20693b4faa4760167be</anchor>
      <arglist>(data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_new</name>
      <anchorfile>namespacemultio__api__data__mod_a841253af9dd200ed010360cd1fd89013.html</anchorfile>
      <anchor>a841253af9dd200ed010360cd1fd89013</anchor>
      <arglist>(data, handle, byte_size)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_delete</name>
      <anchorfile>namespacemultio__api__data__mod_a7247d908ce86b9a4a569a88750e90dc1.html</anchorfile>
      <anchor>a7247d908ce86b9a4a569a88750e90dc1</anchor>
      <arglist>(data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_zero</name>
      <anchorfile>namespacemultio__api__data__mod_afac11605d586eb49c90c194ab0cdcc9a.html</anchorfile>
      <anchor>afac11605d586eb49c90c194ab0cdcc9a</anchor>
      <arglist>(data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_resize</name>
      <anchorfile>namespacemultio__api__data__mod_a5b0f3805c42e6c2d3e67e596738e6085.html</anchorfile>
      <anchor>a5b0f3805c42e6c2d3e67e596738e6085</anchor>
      <arglist>(data, new_size)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_size</name>
      <anchorfile>namespacemultio__api__data__mod_a4bf2691c17489bcae64cd1735a81f162.html</anchorfile>
      <anchor>a4bf2691c17489bcae64cd1735a81f162</anchor>
      <arglist>(data, size)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_byte_size</name>
      <anchorfile>namespacemultio__api__data__mod_a7e15bbc32f815d16a0101640004657b8.html</anchorfile>
      <anchor>a7e15bbc32f815d16a0101640004657b8</anchor>
      <arglist>(data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_set_float_scalar</name>
      <anchorfile>namespacemultio__api__data__mod_a0f0e6c04c1b1878d958a09536e379e2e.html</anchorfile>
      <anchor>a0f0e6c04c1b1878d958a09536e379e2e</anchor>
      <arglist>(data, value, pos)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_set_float_chunk</name>
      <anchorfile>namespacemultio__api__data__mod_a9cf4b967eff018c0b1d6bde271fa09c6.html</anchorfile>
      <anchor>a9cf4b967eff018c0b1d6bde271fa09c6</anchor>
      <arglist>(data, value, pos, size)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_set_double_scalar</name>
      <anchorfile>namespacemultio__api__data__mod_a5be98c1f8c96378e9d1153f6a0c280f0.html</anchorfile>
      <anchor>a5be98c1f8c96378e9d1153f6a0c280f0</anchor>
      <arglist>(data, value, pos)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_set_double_chunk</name>
      <anchorfile>namespacemultio__api__data__mod_a1fa6abb8117c53d64e50b5251461a8c7.html</anchorfile>
      <anchor>a1fa6abb8117c53d64e50b5251461a8c7</anchor>
      <arglist>(data, value, pos, size)</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>multio_fapi_error_handling.F90</name>
    <path>/etc/ecmwf/nfs/dh1_perm_a/mavm/projects/multio-bundle/ifs-bundle/source/multio/src/multio/api/fortran/</path>
    <filename>multio__fapi__error__handling_8F90.html</filename>
    <class kind="type">multio_api_error_handling_mod::multio_failure_info</class>
    <class kind="interface">multio_api_error_handling_mod::failure_handler_t</class>
    <class kind="type">multio_api_error_handling_mod::multio_fort_failure_info_node</class>
    <class kind="type">multio_api_error_handling_mod::multio_fort_failure_info_list</class>
    <namespace>multio_api_error_handling_mod</namespace>
    <member kind="function" protection="private">
      <type>type(c_funptr) function</type>
      <name>multio_fort_failure_wrapper_addr</name>
      <anchorfile>namespacemultio__api__error__handling__mod_aca30b9426edc77ab1473d076c5fc8aef.html</anchorfile>
      <anchor>aca30b9426edc77ab1473d076c5fc8aef</anchor>
      <arglist>(ffi)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>subroutine</type>
      <name>multio_fort_failure_call</name>
      <anchorfile>namespacemultio__api__error__handling__mod_a5eca0d52dac852d569bfe281bdb12081.html</anchorfile>
      <anchor>a5eca0d52dac852d569bfe281bdb12081</anchor>
      <arglist>(ffi, id, err, info)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>type(c_ptr) function</type>
      <name>multio_fort_failure_add</name>
      <anchorfile>namespacemultio__api__error__handling__mod_a1064b10ead27530d4dea323eefb825fe.html</anchorfile>
      <anchor>a1064b10ead27530d4dea323eefb825fe</anchor>
      <arglist>(ffi, handler_fn, context)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>subroutine</type>
      <name>multio_fort_failure_remove</name>
      <anchorfile>namespacemultio__api__error__handling__mod_a1f31b4c9679f3c5463c00fca0687c52a.html</anchorfile>
      <anchor>a1f31b4c9679f3c5463c00fca0687c52a</anchor>
      <arglist>(ffi, id)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>subroutine</type>
      <name>failure_handler_wrapper</name>
      <anchorfile>namespacemultio__api__error__handling__mod_a23eee5e788d3bd1bd59ca393af164c1d.html</anchorfile>
      <anchor>a23eee5e788d3bd1bd59ca393af164c1d</anchor>
      <arglist>(c_pidx, c_error, c_info)</arglist>
    </member>
    <member kind="variable">
      <type>type(multio_fort_failure_info_list), save, public</type>
      <name>failure_info_list</name>
      <anchorfile>namespacemultio__api__error__handling__mod_acc35a8cee743a4363b2da2dd21440b50.html</anchorfile>
      <anchor>acc35a8cee743a4363b2da2dd21440b50</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>multio_fapi_handle.F90</name>
    <path>/etc/ecmwf/nfs/dh1_perm_a/mavm/projects/multio-bundle/ifs-bundle/source/multio/src/multio/api/fortran/</path>
    <filename>multio__fapi__handle_8F90.html</filename>
    <class kind="type">multio_api_handle_mod::multio_handle</class>
    <namespace>multio_api_handle_mod</namespace>
    <member kind="function">
      <type>integer function</type>
      <name>multio_handle_flush</name>
      <anchorfile>namespacemultio__api__handle__mod_a41298c792cf86e0a3679321fe758f5b1.html</anchorfile>
      <anchor>a41298c792cf86e0a3679321fe758f5b1</anchor>
      <arglist>(handle, metadata)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_notify</name>
      <anchorfile>namespacemultio__api__handle__mod_a769f7c8df892a76453d31e1b14e69698.html</anchorfile>
      <anchor>a769f7c8df892a76453d31e1b14e69698</anchor>
      <arglist>(handle, metadata)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_domain</name>
      <anchorfile>namespacemultio__api__handle__mod_a997741f5bd292a104ce4fb2237093625.html</anchorfile>
      <anchor>a997741f5bd292a104ce4fb2237093625</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_mask_float_1d</name>
      <anchorfile>namespacemultio__api__handle__mod_a5a08d82c415200149b0f5562b84cb931.html</anchorfile>
      <anchor>a5a08d82c415200149b0f5562b84cb931</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_mask_float_2d</name>
      <anchorfile>namespacemultio__api__handle__mod_a8d0dbbc8338788e78b8ca2ce8f0cbf9d.html</anchorfile>
      <anchor>a8d0dbbc8338788e78b8ca2ce8f0cbf9d</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_mask_double_1d</name>
      <anchorfile>namespacemultio__api__handle__mod_ac9de24b8dae2bb0fbe3dd77acd09e9b9.html</anchorfile>
      <anchor>ac9de24b8dae2bb0fbe3dd77acd09e9b9</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_mask_double_2d</name>
      <anchorfile>namespacemultio__api__handle__mod_afd53da3b383a2d458f8d72efaf266a5f.html</anchorfile>
      <anchor>afd53da3b383a2d458f8d72efaf266a5f</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_field_float_1d</name>
      <anchorfile>namespacemultio__api__handle__mod_af7ed57fa7c6a6450656eec0b1b7678a1.html</anchorfile>
      <anchor>af7ed57fa7c6a6450656eec0b1b7678a1</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_field_float_2d</name>
      <anchorfile>namespacemultio__api__handle__mod_a076be536739b67187398d81524502af6.html</anchorfile>
      <anchor>a076be536739b67187398d81524502af6</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_field_double_1d</name>
      <anchorfile>namespacemultio__api__handle__mod_a2b5182b01ab31b82b566fa3c2ff4d40a.html</anchorfile>
      <anchor>a2b5182b01ab31b82b566fa3c2ff4d40a</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_field_double_2d</name>
      <anchorfile>namespacemultio__api__handle__mod_a2e1651749003025866e3c3929535d47d.html</anchorfile>
      <anchor>a2e1651749003025866e3c3929535d47d</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_field_buffer</name>
      <anchorfile>namespacemultio__api__handle__mod_a396aa9a98e8e11bb5090564b7192d216.html</anchorfile>
      <anchor>a396aa9a98e8e11bb5090564b7192d216</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_field_accepted</name>
      <anchorfile>namespacemultio__api__handle__mod_a4b69737ee4e7dd553371e00fa9ef16c8.html</anchorfile>
      <anchor>a4b69737ee4e7dd553371e00fa9ef16c8</anchor>
      <arglist>(handle, metadata, set_value)</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>multio_fapi_metadata.F90</name>
    <path>/etc/ecmwf/nfs/dh1_perm_a/mavm/projects/multio-bundle/ifs-bundle/source/multio/src/multio/api/fortran/</path>
    <filename>multio__fapi__metadata_8F90.html</filename>
    <class kind="type">multio_api_metadata_mod::multio_metadata</class>
    <namespace>multio_api_metadata_mod</namespace>
    <member kind="function">
      <type>type(c_ptr) function</type>
      <name>multio_metadata_c_ptr</name>
      <anchorfile>namespacemultio__api__metadata__mod_a971973cf98e879daa7a60697684c0460.html</anchorfile>
      <anchor>a971973cf98e879daa7a60697684c0460</anchor>
      <arglist>(metadata)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_new_metadata</name>
      <anchorfile>namespacemultio__api__metadata__mod_ae5b1b9bb1f65b760e1bdd6be8115097b.html</anchorfile>
      <anchor>ae5b1b9bb1f65b760e1bdd6be8115097b</anchor>
      <arglist>(metadata, handle)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_delete_metadata</name>
      <anchorfile>namespacemultio__api__metadata__mod_adb215b35fc2c5ff634693d3145e58bdc.html</anchorfile>
      <anchor>adb215b35fc2c5ff634693d3145e58bdc</anchor>
      <arglist>(metadata)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_metadata_set_string</name>
      <anchorfile>namespacemultio__api__metadata__mod_aa2d9a6447210740faf19234ccac916c0.html</anchorfile>
      <anchor>aa2d9a6447210740faf19234ccac916c0</anchor>
      <arglist>(metadata, key, value)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_metadata_set_int8</name>
      <anchorfile>namespacemultio__api__metadata__mod_a82ad5b3807f008e26d1513e34d99b99a.html</anchorfile>
      <anchor>a82ad5b3807f008e26d1513e34d99b99a</anchor>
      <arglist>(metadata, key, value)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_metadata_set_int16</name>
      <anchorfile>namespacemultio__api__metadata__mod_a3e6e39d39cddac5b86968cd9a2c3aea7.html</anchorfile>
      <anchor>a3e6e39d39cddac5b86968cd9a2c3aea7</anchor>
      <arglist>(metadata, key, value)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_metadata_set_int32</name>
      <anchorfile>namespacemultio__api__metadata__mod_aeabcb4a240124df96d498ed1f8d3a87b.html</anchorfile>
      <anchor>aeabcb4a240124df96d498ed1f8d3a87b</anchor>
      <arglist>(metadata, key, value)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_metadata_set_int64</name>
      <anchorfile>namespacemultio__api__metadata__mod_af0c8b3075db40897cec631a177245c21.html</anchorfile>
      <anchor>af0c8b3075db40897cec631a177245c21</anchor>
      <arglist>(metadata, key, value)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_metadata_set_real32</name>
      <anchorfile>namespacemultio__api__metadata__mod_ae52812a0c75e95e1e4e87069dc0c6c03.html</anchorfile>
      <anchor>ae52812a0c75e95e1e4e87069dc0c6c03</anchor>
      <arglist>(metadata, key, value)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_metadata_set_real64</name>
      <anchorfile>namespacemultio__api__metadata__mod_a0184c60d78f2d067d962bccc1f4dfb28.html</anchorfile>
      <anchor>a0184c60d78f2d067d962bccc1f4dfb28</anchor>
      <arglist>(metadata, key, value)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_metadata_set_fbool</name>
      <anchorfile>namespacemultio__api__metadata__mod_a66547e680758c94b3dcec96c2a7ced13.html</anchorfile>
      <anchor>a66547e680758c94b3dcec96c2a7ced13</anchor>
      <arglist>(metadata, key, value)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_metadata_set_cbool</name>
      <anchorfile>namespacemultio__api__metadata__mod_a00f6d3037b5c69b7f110c3f4d72ccaf2.html</anchorfile>
      <anchor>a00f6d3037b5c69b7f110c3f4d72ccaf2</anchor>
      <arglist>(metadata, key, value)</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>multio_fapi_utils.F90</name>
    <path>/etc/ecmwf/nfs/dh1_perm_a/mavm/projects/multio-bundle/ifs-bundle/source/multio/src/multio/api/fortran/</path>
    <filename>multio__fapi__utils_8F90.html</filename>
    <namespace>multio_api_utils_mod</namespace>
    <member kind="function">
      <type>integer function, public</type>
      <name>multio_initialise</name>
      <anchorfile>namespacemultio__api__utils__mod_aca5fc0af011587af64e09292604027f9.html</anchorfile>
      <anchor>aca5fc0af011587af64e09292604027f9</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>integer function, public</type>
      <name>multio_start_server</name>
      <anchorfile>namespacemultio__api__utils__mod_ab9faa722cc754d73c10a4383526e79ea.html</anchorfile>
      <anchor>ab9faa722cc754d73c10a4383526e79ea</anchor>
      <arglist>(cc)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>character(:) function, allocatable, target</type>
      <name>fortranise_cstr</name>
      <anchorfile>namespacemultio__api__utils__mod_a673954c16db544449efac701d2d0dbaf.html</anchorfile>
      <anchor>a673954c16db544449efac701d2d0dbaf</anchor>
      <arglist>(cstr)</arglist>
    </member>
    <member kind="function">
      <type>integer function, public</type>
      <name>multio_version</name>
      <anchorfile>namespacemultio__api__utils__mod_a268b6fb31ec6cea924d59a71baa22a67.html</anchorfile>
      <anchor>a268b6fb31ec6cea924d59a71baa22a67</anchor>
      <arglist>(version_str)</arglist>
    </member>
    <member kind="function">
      <type>integer function, public</type>
      <name>multio_vcs_version</name>
      <anchorfile>namespacemultio__api__utils__mod_a93516c58d996e482cbc442a21147caf1.html</anchorfile>
      <anchor>a93516c58d996e482cbc442a21147caf1</anchor>
      <arglist>(git_sha1)</arglist>
    </member>
    <member kind="function">
      <type>character(:) function, allocatable, target, public</type>
      <name>multio_error_string</name>
      <anchorfile>namespacemultio__api__utils__mod_a1b6859bc9d00ae073164000041931fe7.html</anchorfile>
      <anchor>a1b6859bc9d00ae073164000041931fe7</anchor>
      <arglist>(err, info)</arglist>
    </member>
  </compound>
  <compound kind="module">
    <name>datatype</name>
    <filename>classdatatype.html</filename>
  </compound>
  <compound kind="interface">
    <name>multio_api_error_handling_mod::failure_handler_t</name>
    <filename>interfacemultio__api__error__handling__mod_1_1failure__handler__t.html</filename>
    <member kind="function" virtualness="virtual">
      <type>subroutine</type>
      <name>failure_handler_t</name>
      <anchorfile>interfacemultio__api__error__handling__mod_1_1failure__handler__t_a2ff15ea3b3cab28e97c6588f693b8d6f.html</anchorfile>
      <anchor>a2ff15ea3b3cab28e97c6588f693b8d6f</anchor>
      <arglist>(context, error, info)</arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>multio_api_base_handle_mod::multio_base_handle</name>
    <filename>structmultio__api__base__handle__mod_1_1multio__base__handle.html</filename>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>delete</name>
      <anchorfile>structmultio__api__base__handle__mod_1_1multio__base__handle_aa451d265f656a0e1d056646ad3266f61.html</anchorfile>
      <anchor>aa451d265f656a0e1d056646ad3266f61</anchor>
      <arglist>=&gt; multio_base_handle_delete</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>c_ptr</name>
      <anchorfile>structmultio__api__base__handle__mod_1_1multio__base__handle_a02d9fc7ad084e3554b7ef20649acd886.html</anchorfile>
      <anchor>a02d9fc7ad084e3554b7ef20649acd886</anchor>
      <arglist>=&gt; multio_base_handle_c_ptr</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>set_failure_handler</name>
      <anchorfile>structmultio__api__base__handle__mod_1_1multio__base__handle_a439339af6fadcab0bd38380324d5b751.html</anchorfile>
      <anchor>a439339af6fadcab0bd38380324d5b751</anchor>
      <arglist>=&gt; multio_base_handle_set_failure_handler</arglist>
    </member>
    <member kind="function">
      <type>generic, public</type>
      <name>new</name>
      <anchorfile>structmultio__api__base__handle__mod_1_1multio__base__handle_a950c30dd182c35bf9032e5329986941f.html</anchorfile>
      <anchor>a950c30dd182c35bf9032e5329986941f</anchor>
      <arglist>=&gt; new_handle, new_handle_default</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>open_connections</name>
      <anchorfile>structmultio__api__base__handle__mod_1_1multio__base__handle_acd1a9cc4036df3b76515a408d4eda25b.html</anchorfile>
      <anchor>acd1a9cc4036df3b76515a408d4eda25b</anchor>
      <arglist>=&gt; multio_base_handle_open_connections</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>close_connections</name>
      <anchorfile>structmultio__api__base__handle__mod_1_1multio__base__handle_af8add0368312da2ed9ad8f346ab4620d.html</anchorfile>
      <anchor>af8add0368312da2ed9ad8f346ab4620d</anchor>
      <arglist>=&gt; multio_base_handle_close_connections</arglist>
    </member>
    <member kind="variable">
      <type>type(c_ptr)</type>
      <name>impl</name>
      <anchorfile>structmultio__api__base__handle__mod_1_1multio__base__handle_a13e33ee607020b3e87de1d7bd09dfdee.html</anchorfile>
      <anchor>a13e33ee607020b3e87de1d7bd09dfdee</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer(c_int), pointer</type>
      <name>failure_id</name>
      <anchorfile>structmultio__api__base__handle__mod_1_1multio__base__handle_aa9ba196fee772ec38660821536a72d76.html</anchorfile>
      <anchor>aa9ba196fee772ec38660821536a72d76</anchor>
      <arglist></arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>new_handle</name>
      <anchorfile>structmultio__api__base__handle__mod_1_1multio__base__handle_aa18c43e72dc55cbfa8667c745b84b7a8.html</anchorfile>
      <anchor>aa18c43e72dc55cbfa8667c745b84b7a8</anchor>
      <arglist>=&gt; multio_base_handle_new</arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>new_handle_default</name>
      <anchorfile>structmultio__api__base__handle__mod_1_1multio__base__handle_aad64ab0a20a2493baefa4f8a27424106.html</anchorfile>
      <anchor>aad64ab0a20a2493baefa4f8a27424106</anchor>
      <arglist>=&gt; multio_base_handle_new_default</arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>multio_api_configuration_mod::multio_configuration</name>
    <filename>structmultio__api__configuration__mod_1_1multio__configuration.html</filename>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>new_default</name>
      <anchorfile>structmultio__api__configuration__mod_1_1multio__configuration_ad5d7474f91f4f7d3e3e1f64e5be6cc1c.html</anchorfile>
      <anchor>ad5d7474f91f4f7d3e3e1f64e5be6cc1c</anchor>
      <arglist>=&gt; multio_new_configuration</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>new_from_filename</name>
      <anchorfile>structmultio__api__configuration__mod_1_1multio__configuration_a81127c1523e7a5d46a070c851530236a.html</anchorfile>
      <anchor>a81127c1523e7a5d46a070c851530236a</anchor>
      <arglist>=&gt; multio_new_configuration_from_filename</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>c_ptr</name>
      <anchorfile>structmultio__api__configuration__mod_1_1multio__configuration_abe960ebbec79398767b6a2b7c9063277.html</anchorfile>
      <anchor>abe960ebbec79398767b6a2b7c9063277</anchor>
      <arglist>=&gt; multio_configuration_c_ptr</arglist>
    </member>
    <member kind="function">
      <type>generic, public</type>
      <name>new</name>
      <anchorfile>structmultio__api__configuration__mod_1_1multio__configuration_aafb7ce5389e756e055eb3896a481c12b.html</anchorfile>
      <anchor>aafb7ce5389e756e055eb3896a481c12b</anchor>
      <arglist>=&gt; new_default, new_from_filename</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>delete</name>
      <anchorfile>structmultio__api__configuration__mod_1_1multio__configuration_a08714ec02bacaabca04e3698c733810e.html</anchorfile>
      <anchor>a08714ec02bacaabca04e3698c733810e</anchor>
      <arglist>=&gt; multio_delete_configuration</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>set_failure_handler</name>
      <anchorfile>structmultio__api__configuration__mod_1_1multio__configuration_a6117fb20f8505bf8e697bd736aecbbea.html</anchorfile>
      <anchor>a6117fb20f8505bf8e697bd736aecbbea</anchor>
      <arglist>=&gt; multio_conf_set_failure_handler</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>move_failure_id</name>
      <anchorfile>structmultio__api__configuration__mod_1_1multio__configuration_ac2f0eabfb38e1d718ec279315289fa1f.html</anchorfile>
      <anchor>ac2f0eabfb38e1d718ec279315289fa1f</anchor>
      <arglist>=&gt; multio_conf_move_failure_id</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>set_path</name>
      <anchorfile>structmultio__api__configuration__mod_1_1multio__configuration_afe5adedc6b40d1e3e6a493700adbfdba.html</anchorfile>
      <anchor>afe5adedc6b40d1e3e6a493700adbfdba</anchor>
      <arglist>=&gt; multio_conf_set_path</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>mpi_allow_world_default_comm</name>
      <anchorfile>structmultio__api__configuration__mod_1_1multio__configuration_a6f0ddbc47df78abd30a1248d28a1e480.html</anchorfile>
      <anchor>a6f0ddbc47df78abd30a1248d28a1e480</anchor>
      <arglist>=&gt; multio_conf_mpi_allow_world_default_comm</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>mpi_parent_comm</name>
      <anchorfile>structmultio__api__configuration__mod_1_1multio__configuration_ab5ca797e0584a6156aa7e82c2b346cfa.html</anchorfile>
      <anchor>ab5ca797e0584a6156aa7e82c2b346cfa</anchor>
      <arglist>=&gt; multio_conf_mpi_parent_comm</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>mpi_return_client_comm</name>
      <anchorfile>structmultio__api__configuration__mod_1_1multio__configuration_a70e2054ccfcc0ee12641ee6ea9ab85f8.html</anchorfile>
      <anchor>a70e2054ccfcc0ee12641ee6ea9ab85f8</anchor>
      <arglist>=&gt; multio_conf_mpi_return_client_comm</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>mpi_return_server_comm</name>
      <anchorfile>structmultio__api__configuration__mod_1_1multio__configuration_abbd439a56143e54da52cd906a45a100b.html</anchorfile>
      <anchor>abbd439a56143e54da52cd906a45a100b</anchor>
      <arglist>=&gt; multio_conf_mpi_return_server_comm</arglist>
    </member>
    <member kind="variable">
      <type>integer(c_int), pointer</type>
      <name>failure_id</name>
      <anchorfile>structmultio__api__configuration__mod_1_1multio__configuration_ac07c92d5d434e996c0bb10f43297de1a.html</anchorfile>
      <anchor>ac07c92d5d434e996c0bb10f43297de1a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="private">
      <type>type(c_ptr)</type>
      <name>impl</name>
      <anchorfile>structmultio__api__configuration__mod_1_1multio__configuration_a685e63d490408ea7251a2a1845d99bbc.html</anchorfile>
      <anchor>a685e63d490408ea7251a2a1845d99bbc</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>multio_api_data_mod::multio_data</name>
    <filename>structmultio__api__data__mod_1_1multio__data.html</filename>
    <member kind="function">
      <type>procedure</type>
      <name>new</name>
      <anchorfile>structmultio__api__data__mod_1_1multio__data_aacf12b2005f6d99b72cc23e725ad94b1.html</anchorfile>
      <anchor>aacf12b2005f6d99b72cc23e725ad94b1</anchor>
      <arglist>=&gt; multio_data_new</arglist>
    </member>
    <member kind="function">
      <type>procedure</type>
      <name>delete</name>
      <anchorfile>structmultio__api__data__mod_1_1multio__data_a4bf2bf501cfa55452767e919b2b197ef.html</anchorfile>
      <anchor>a4bf2bf501cfa55452767e919b2b197ef</anchor>
      <arglist>=&gt; multio_data_delete</arglist>
    </member>
    <member kind="function">
      <type>procedure</type>
      <name>c_ptr</name>
      <anchorfile>structmultio__api__data__mod_1_1multio__data_aa1366dc1ac517550682e73c1e2abae18.html</anchorfile>
      <anchor>aa1366dc1ac517550682e73c1e2abae18</anchor>
      <arglist>=&gt; multio_data_c_ptr</arglist>
    </member>
    <member kind="function">
      <type>procedure</type>
      <name>resize</name>
      <anchorfile>structmultio__api__data__mod_1_1multio__data_a099a0c10731af541f82bf00b654fe999.html</anchorfile>
      <anchor>a099a0c10731af541f82bf00b654fe999</anchor>
      <arglist>=&gt; multio_data_resize</arglist>
    </member>
    <member kind="function">
      <type>procedure</type>
      <name>size</name>
      <anchorfile>structmultio__api__data__mod_1_1multio__data_a82c60424b87b92cd3fb9dd7f382a2ab5.html</anchorfile>
      <anchor>a82c60424b87b92cd3fb9dd7f382a2ab5</anchor>
      <arglist>=&gt; multio_data_size</arglist>
    </member>
    <member kind="function">
      <type>procedure</type>
      <name>zero</name>
      <anchorfile>structmultio__api__data__mod_1_1multio__data_aaaa84185bf728c2f30a0840d8f864109.html</anchorfile>
      <anchor>aaaa84185bf728c2f30a0840d8f864109</anchor>
      <arglist>=&gt; multio_data_zero</arglist>
    </member>
    <member kind="function">
      <type>procedure</type>
      <name>byte_size</name>
      <anchorfile>structmultio__api__data__mod_1_1multio__data_acfce5962ea9e95862a87afe6ab211695.html</anchorfile>
      <anchor>acfce5962ea9e95862a87afe6ab211695</anchor>
      <arglist>=&gt; multio_data_byte_size</arglist>
    </member>
    <member kind="function">
      <type>procedure</type>
      <name>set_float_scalar</name>
      <anchorfile>structmultio__api__data__mod_1_1multio__data_abe0c0ba65815a60b00bc0556c94efda7.html</anchorfile>
      <anchor>abe0c0ba65815a60b00bc0556c94efda7</anchor>
      <arglist>=&gt; multio_data_set_float_scalar</arglist>
    </member>
    <member kind="function">
      <type>procedure</type>
      <name>set_double_scalar</name>
      <anchorfile>structmultio__api__data__mod_1_1multio__data_af229fa242d93702bd60f340ceb0d6598.html</anchorfile>
      <anchor>af229fa242d93702bd60f340ceb0d6598</anchor>
      <arglist>=&gt; multio_data_set_double_scalar</arglist>
    </member>
    <member kind="function">
      <type>procedure</type>
      <name>set_float_chunk</name>
      <anchorfile>structmultio__api__data__mod_1_1multio__data_aea2894272786a9525d57a0f1592f401c.html</anchorfile>
      <anchor>aea2894272786a9525d57a0f1592f401c</anchor>
      <arglist>=&gt; multio_data_set_float_chunk</arglist>
    </member>
    <member kind="function">
      <type>procedure</type>
      <name>set_double_chunk</name>
      <anchorfile>structmultio__api__data__mod_1_1multio__data_a67610cd3230791c29c1f6d479176e3ca.html</anchorfile>
      <anchor>a67610cd3230791c29c1f6d479176e3ca</anchor>
      <arglist>=&gt; multio_data_set_double_chunk</arglist>
    </member>
    <member kind="function">
      <type>generic</type>
      <name>set</name>
      <anchorfile>structmultio__api__data__mod_1_1multio__data_a7a93657b691c5c001fb96c0438e356d1.html</anchorfile>
      <anchor>a7a93657b691c5c001fb96c0438e356d1</anchor>
      <arglist>=&gt; set_float_scalar, set_double_scalar, set_float_chunk, set_double_chunk</arglist>
    </member>
    <member kind="variable">
      <type>integer(kind=c_int)</type>
      <name>byte_size_</name>
      <anchorfile>structmultio__api__data__mod_1_1multio__data_abfa1230246d9f1782d1af58f05996acd.html</anchorfile>
      <anchor>abfa1230246d9f1782d1af58f05996acd</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="private">
      <type>type(c_ptr)</type>
      <name>impl</name>
      <anchorfile>structmultio__api__data__mod_1_1multio__data_a5161ce1bf4a8339f9668b65f1c3d7240.html</anchorfile>
      <anchor>a5161ce1bf4a8339f9668b65f1c3d7240</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>multio_api_error_handling_mod::multio_failure_info</name>
    <filename>structmultio__api__error__handling__mod_1_1multio__failure__info.html</filename>
    <member kind="variable">
      <type>type(c_ptr)</type>
      <name>impl</name>
      <anchorfile>structmultio__api__error__handling__mod_1_1multio__failure__info_aa2bb6c108dea59bd557f4d183ffec42a.html</anchorfile>
      <anchor>aa2bb6c108dea59bd557f4d183ffec42a</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>multio_api_error_handling_mod::multio_fort_failure_info_list</name>
    <filename>structmultio__api__error__handling__mod_1_1multio__fort__failure__info__list.html</filename>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>callhandler</name>
      <anchorfile>structmultio__api__error__handling__mod_1_1multio__fort__failure__info__list_a976f965c9ab78ee48efdab6eb30b0627.html</anchorfile>
      <anchor>a976f965c9ab78ee48efdab6eb30b0627</anchor>
      <arglist>=&gt; multio_fort_failure_call</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>add</name>
      <anchorfile>structmultio__api__error__handling__mod_1_1multio__fort__failure__info__list_a6bec3bb628a93625c474a01d5fb5a1bd.html</anchorfile>
      <anchor>a6bec3bb628a93625c474a01d5fb5a1bd</anchor>
      <arglist>=&gt; multio_fort_failure_add</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>remove</name>
      <anchorfile>structmultio__api__error__handling__mod_1_1multio__fort__failure__info__list_a86f9454e6b8cd152a464438f8df132fe.html</anchorfile>
      <anchor>a86f9454e6b8cd152a464438f8df132fe</anchor>
      <arglist>=&gt; multio_fort_failure_remove</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>c_wrapper</name>
      <anchorfile>structmultio__api__error__handling__mod_1_1multio__fort__failure__info__list_afb2232a7cdf21b8bf4820cc058a75807.html</anchorfile>
      <anchor>afb2232a7cdf21b8bf4820cc058a75807</anchor>
      <arglist>=&gt; multio_fort_failure_wrapper_addr</arglist>
    </member>
    <member kind="variable">
      <type>integer</type>
      <name>count</name>
      <anchorfile>structmultio__api__error__handling__mod_1_1multio__fort__failure__info__list_aaa9fb83d1f379268e011aec4ef6dce85.html</anchorfile>
      <anchor>aaa9fb83d1f379268e011aec4ef6dce85</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>type(multio_fort_failure_info_node), pointer</type>
      <name>head</name>
      <anchorfile>structmultio__api__error__handling__mod_1_1multio__fort__failure__info__list_a8f35367a077c7778f56924f63fc8724a.html</anchorfile>
      <anchor>a8f35367a077c7778f56924f63fc8724a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>type(multio_fort_failure_info_node), pointer</type>
      <name>tail</name>
      <anchorfile>structmultio__api__error__handling__mod_1_1multio__fort__failure__info__list_a63a74b28b75de4007007ad9912a1a117.html</anchorfile>
      <anchor>a63a74b28b75de4007007ad9912a1a117</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="private">
      <type>integer(c_int)</type>
      <name>lastid</name>
      <anchorfile>structmultio__api__error__handling__mod_1_1multio__fort__failure__info__list_aff790f33ae86a520396f0ce5a7fa0fd7.html</anchorfile>
      <anchor>aff790f33ae86a520396f0ce5a7fa0fd7</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>multio_api_error_handling_mod::multio_fort_failure_info_node</name>
    <filename>structmultio__api__error__handling__mod_1_1multio__fort__failure__info__node.html</filename>
    <member kind="variable">
      <type>integer(c_int)</type>
      <name>id</name>
      <anchorfile>structmultio__api__error__handling__mod_1_1multio__fort__failure__info__node_adeca63d52f521cf905cfb05c337cf962.html</anchorfile>
      <anchor>adeca63d52f521cf905cfb05c337cf962</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer(int64)</type>
      <name>context</name>
      <anchorfile>structmultio__api__error__handling__mod_1_1multio__fort__failure__info__node_a6af071d3d5068355257464e9e3b93a85.html</anchorfile>
      <anchor>a6af071d3d5068355257464e9e3b93a85</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>procedure(failure_handler_t), pointer, nopass</type>
      <name>handler_fn</name>
      <anchorfile>structmultio__api__error__handling__mod_1_1multio__fort__failure__info__node_ac412346f410e2e1d363c96eb5ce39836.html</anchorfile>
      <anchor>ac412346f410e2e1d363c96eb5ce39836</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>type(multio_fort_failure_info_node), pointer</type>
      <name>next</name>
      <anchorfile>structmultio__api__error__handling__mod_1_1multio__fort__failure__info__node_a24f02fdfb4de85dc8c17145cc13ca42c.html</anchorfile>
      <anchor>a24f02fdfb4de85dc8c17145cc13ca42c</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>multio_api_handle_mod::multio_handle</name>
    <filename>structmultio__api__handle__mod_1_1multio__handle.html</filename>
    <base>multio_api_base_handle_mod::multio_base_handle</base>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>flush</name>
      <anchorfile>structmultio__api__handle__mod_1_1multio__handle_aa74c0e14219d6ef3b3a630e853bc78e7.html</anchorfile>
      <anchor>aa74c0e14219d6ef3b3a630e853bc78e7</anchor>
      <arglist>=&gt; multio_handle_flush</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>notify</name>
      <anchorfile>structmultio__api__handle__mod_1_1multio__handle_ad9e9f21da6ee4241fac5ba462db8d205.html</anchorfile>
      <anchor>ad9e9f21da6ee4241fac5ba462db8d205</anchor>
      <arglist>=&gt; multio_handle_notify</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>write_domain</name>
      <anchorfile>structmultio__api__handle__mod_1_1multio__handle_a8366295ceca8f234c14e6323398c1b1f.html</anchorfile>
      <anchor>a8366295ceca8f234c14e6323398c1b1f</anchor>
      <arglist>=&gt; multio_handle_write_domain</arglist>
    </member>
    <member kind="function">
      <type>generic, public</type>
      <name>write_mask</name>
      <anchorfile>structmultio__api__handle__mod_1_1multio__handle_a541372f5da585a463e53586dfa1d343a.html</anchorfile>
      <anchor>a541372f5da585a463e53586dfa1d343a</anchor>
      <arglist>=&gt; write_mask_float_1d, write_mask_float_2d, write_mask_double_1d, write_mask_double_2d</arglist>
    </member>
    <member kind="function">
      <type>generic, public</type>
      <name>write_field</name>
      <anchorfile>structmultio__api__handle__mod_1_1multio__handle_a593fb9d5c69665cc598824f07ce3b899.html</anchorfile>
      <anchor>a593fb9d5c69665cc598824f07ce3b899</anchor>
      <arglist>=&gt; write_field_float_1d, write_field_float_2d, write_field_double_1d, write_field_double_2d, write_field_buffer</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>field_accepted</name>
      <anchorfile>structmultio__api__handle__mod_1_1multio__handle_a3da36ff9c4d68cc9ad96375717267560.html</anchorfile>
      <anchor>a3da36ff9c4d68cc9ad96375717267560</anchor>
      <arglist>=&gt; multio_handle_field_accepted</arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>write_mask_float_1d</name>
      <anchorfile>structmultio__api__handle__mod_1_1multio__handle_ac923da165d4070aa380940e8495433b9.html</anchorfile>
      <anchor>ac923da165d4070aa380940e8495433b9</anchor>
      <arglist>=&gt; multio_handle_write_mask_float_1d</arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>write_mask_double_1d</name>
      <anchorfile>structmultio__api__handle__mod_1_1multio__handle_a334ceef4387b54ce21fff786fc28d6b7.html</anchorfile>
      <anchor>a334ceef4387b54ce21fff786fc28d6b7</anchor>
      <arglist>=&gt; multio_handle_write_mask_double_1d</arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>write_mask_float_2d</name>
      <anchorfile>structmultio__api__handle__mod_1_1multio__handle_a507d8c9a87b52b8f02614a8bbec00d79.html</anchorfile>
      <anchor>a507d8c9a87b52b8f02614a8bbec00d79</anchor>
      <arglist>=&gt; multio_handle_write_mask_float_2d</arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>write_mask_double_2d</name>
      <anchorfile>structmultio__api__handle__mod_1_1multio__handle_a018a5e37f48bcd037058a545528dd842.html</anchorfile>
      <anchor>a018a5e37f48bcd037058a545528dd842</anchor>
      <arglist>=&gt; multio_handle_write_mask_double_2d</arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>write_field_buffer</name>
      <anchorfile>structmultio__api__handle__mod_1_1multio__handle_a38346c87b77e396b2c72de710f16460d.html</anchorfile>
      <anchor>a38346c87b77e396b2c72de710f16460d</anchor>
      <arglist>=&gt; multio_handle_write_field_buffer</arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>write_field_float_1d</name>
      <anchorfile>structmultio__api__handle__mod_1_1multio__handle_a98a27ebb0bd1288fc10357e8a4020825.html</anchorfile>
      <anchor>a98a27ebb0bd1288fc10357e8a4020825</anchor>
      <arglist>=&gt; multio_handle_write_field_float_1d</arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>write_field_double_1d</name>
      <anchorfile>structmultio__api__handle__mod_1_1multio__handle_af4264ce22ac322d857dd46956837965c.html</anchorfile>
      <anchor>af4264ce22ac322d857dd46956837965c</anchor>
      <arglist>=&gt; multio_handle_write_field_double_1d</arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>write_field_float_2d</name>
      <anchorfile>structmultio__api__handle__mod_1_1multio__handle_a8c36dc5527a13338653110c0037e152f.html</anchorfile>
      <anchor>a8c36dc5527a13338653110c0037e152f</anchor>
      <arglist>=&gt; multio_handle_write_field_float_2d</arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>write_field_double_2d</name>
      <anchorfile>structmultio__api__handle__mod_1_1multio__handle_ae5f4e596a39206c1e3d30c63f037bfc6.html</anchorfile>
      <anchor>ae5f4e596a39206c1e3d30c63f037bfc6</anchor>
      <arglist>=&gt; multio_handle_write_field_double_2d</arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>multio_api_metadata_mod::multio_metadata</name>
    <filename>structmultio__api__metadata__mod_1_1multio__metadata.html</filename>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>new</name>
      <anchorfile>structmultio__api__metadata__mod_1_1multio__metadata_aa6632f151badc73dc699760a856f8d59.html</anchorfile>
      <anchor>aa6632f151badc73dc699760a856f8d59</anchor>
      <arglist>=&gt; multio_new_metadata</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>delete</name>
      <anchorfile>structmultio__api__metadata__mod_1_1multio__metadata_ab7a4420840330170b9cdab423cc29c29.html</anchorfile>
      <anchor>ab7a4420840330170b9cdab423cc29c29</anchor>
      <arglist>=&gt; multio_delete_metadata</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>c_ptr</name>
      <anchorfile>structmultio__api__metadata__mod_1_1multio__metadata_a93ea57880825a83f754d1b9c5e224db8.html</anchorfile>
      <anchor>a93ea57880825a83f754d1b9c5e224db8</anchor>
      <arglist>=&gt; multio_metadata_c_ptr</arglist>
    </member>
    <member kind="function">
      <type>procedure, pass, public</type>
      <name>set_string</name>
      <anchorfile>structmultio__api__metadata__mod_1_1multio__metadata_a7dc55090f1c2bfce8241ffb373cc5d48.html</anchorfile>
      <anchor>a7dc55090f1c2bfce8241ffb373cc5d48</anchor>
      <arglist>=&gt; multio_metadata_set_string</arglist>
    </member>
    <member kind="function">
      <type>generic, public</type>
      <name>set_int</name>
      <anchorfile>structmultio__api__metadata__mod_1_1multio__metadata_a532ef05af9765e00a7be5ddff66649a1.html</anchorfile>
      <anchor>a532ef05af9765e00a7be5ddff66649a1</anchor>
      <arglist>=&gt; set_int8, set_int16, set_int32, set_int64</arglist>
    </member>
    <member kind="function">
      <type>generic, public</type>
      <name>set_bool</name>
      <anchorfile>structmultio__api__metadata__mod_1_1multio__metadata_a65675fe0dbac69c48bd9a4cc476d5dda.html</anchorfile>
      <anchor>a65675fe0dbac69c48bd9a4cc476d5dda</anchor>
      <arglist>=&gt; set_cbool, set_fbool</arglist>
    </member>
    <member kind="function">
      <type>generic, public</type>
      <name>set_real</name>
      <anchorfile>structmultio__api__metadata__mod_1_1multio__metadata_ae9a590f4d1b45f3d8a28a197ea45c016.html</anchorfile>
      <anchor>ae9a590f4d1b45f3d8a28a197ea45c016</anchor>
      <arglist>=&gt; set_real32, set_real64</arglist>
    </member>
    <member kind="function">
      <type>generic, public</type>
      <name>set</name>
      <anchorfile>structmultio__api__metadata__mod_1_1multio__metadata_abe4833f3e69811ae3457ded03e15ee39.html</anchorfile>
      <anchor>abe4833f3e69811ae3457ded03e15ee39</anchor>
      <arglist>=&gt; set_string, set_int8, set_int16, set_int32, set_int64, set_cbool, set_fbool, set_real32, set_real64</arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>set_int8</name>
      <anchorfile>structmultio__api__metadata__mod_1_1multio__metadata_a2b5fec7c57b9b805b3ff16922e84e32a.html</anchorfile>
      <anchor>a2b5fec7c57b9b805b3ff16922e84e32a</anchor>
      <arglist>=&gt; multio_metadata_set_int8</arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>set_int16</name>
      <anchorfile>structmultio__api__metadata__mod_1_1multio__metadata_ab70bc38ca8e1f876c1a9247d48d3fa62.html</anchorfile>
      <anchor>ab70bc38ca8e1f876c1a9247d48d3fa62</anchor>
      <arglist>=&gt; multio_metadata_set_int16</arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>set_int32</name>
      <anchorfile>structmultio__api__metadata__mod_1_1multio__metadata_a8a1d3ae0b1e320c361766c4e4816a965.html</anchorfile>
      <anchor>a8a1d3ae0b1e320c361766c4e4816a965</anchor>
      <arglist>=&gt; multio_metadata_set_int32</arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>set_int64</name>
      <anchorfile>structmultio__api__metadata__mod_1_1multio__metadata_acec1b8bcd04e2c87f2557ac0422011a4.html</anchorfile>
      <anchor>acec1b8bcd04e2c87f2557ac0422011a4</anchor>
      <arglist>=&gt; multio_metadata_set_int64</arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>set_fbool</name>
      <anchorfile>structmultio__api__metadata__mod_1_1multio__metadata_a73d64849cfd7fc096cc8f40c2fa61ae8.html</anchorfile>
      <anchor>a73d64849cfd7fc096cc8f40c2fa61ae8</anchor>
      <arglist>=&gt; multio_metadata_set_fbool</arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>set_cbool</name>
      <anchorfile>structmultio__api__metadata__mod_1_1multio__metadata_aca91593ce27c7069f799e87a278df466.html</anchorfile>
      <anchor>aca91593ce27c7069f799e87a278df466</anchor>
      <arglist>=&gt; multio_metadata_set_cbool</arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>set_real32</name>
      <anchorfile>structmultio__api__metadata__mod_1_1multio__metadata_ab117374023be9fcc716f1aa543533c7d.html</anchorfile>
      <anchor>ab117374023be9fcc716f1aa543533c7d</anchor>
      <arglist>=&gt; multio_metadata_set_real32</arglist>
    </member>
    <member kind="function" protection="private">
      <type>procedure, pass, private</type>
      <name>set_real64</name>
      <anchorfile>structmultio__api__metadata__mod_1_1multio__metadata_ae9d5fa474a8175397757c03535676766.html</anchorfile>
      <anchor>ae9d5fa474a8175397757c03535676766</anchor>
      <arglist>=&gt; multio_metadata_set_real64</arglist>
    </member>
    <member kind="variable" protection="private">
      <type>type(c_ptr)</type>
      <name>impl</name>
      <anchorfile>structmultio__api__metadata__mod_1_1multio__metadata_ae98950903937120d8db36d326e6cecd3.html</anchorfile>
      <anchor>ae98950903937120d8db36d326e6cecd3</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>multio_api</name>
    <filename>namespacemultio__api.html</filename>
  </compound>
  <compound kind="namespace">
    <name>multio_api_base_handle_mod</name>
    <filename>namespacemultio__api__base__handle__mod.html</filename>
    <class kind="type">multio_api_base_handle_mod::multio_base_handle</class>
    <member kind="function">
      <type>type(c_ptr) function</type>
      <name>multio_base_handle_c_ptr</name>
      <anchorfile>namespacemultio__api__base__handle__mod_afbf109b8fff00ce6024bd4ec18eed64d.html</anchorfile>
      <anchor>afbf109b8fff00ce6024bd4ec18eed64d</anchor>
      <arglist>(handle)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_base_handle_new</name>
      <anchorfile>namespacemultio__api__base__handle__mod_a303bc6624a0fd1d4812bb2f4c5b154cf.html</anchorfile>
      <anchor>a303bc6624a0fd1d4812bb2f4c5b154cf</anchor>
      <arglist>(handle, cc)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_base_handle_new_default</name>
      <anchorfile>namespacemultio__api__base__handle__mod_aeeb92733e88a8194bae18279ce600062.html</anchorfile>
      <anchor>aeeb92733e88a8194bae18279ce600062</anchor>
      <arglist>(handle)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_base_handle_delete</name>
      <anchorfile>namespacemultio__api__base__handle__mod_aa8208cf8ac30c02b14fe6bd3c3ab233a.html</anchorfile>
      <anchor>aa8208cf8ac30c02b14fe6bd3c3ab233a</anchor>
      <arglist>(handle)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_base_handle_set_failure_handler</name>
      <anchorfile>namespacemultio__api__base__handle__mod_a6fc73e6606f0f89e0ee9391974153633.html</anchorfile>
      <anchor>a6fc73e6606f0f89e0ee9391974153633</anchor>
      <arglist>(handle, handler, context)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_base_handle_open_connections</name>
      <anchorfile>namespacemultio__api__base__handle__mod_a9d7aa8a221614ef76a9cacf4ecb07307.html</anchorfile>
      <anchor>a9d7aa8a221614ef76a9cacf4ecb07307</anchor>
      <arglist>(handle)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_base_handle_close_connections</name>
      <anchorfile>namespacemultio__api__base__handle__mod_ad745a00ea651391354b017e56293f6ed.html</anchorfile>
      <anchor>ad745a00ea651391354b017e56293f6ed</anchor>
      <arglist>(handle)</arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>multio_api_configuration_mod</name>
    <filename>namespacemultio__api__configuration__mod.html</filename>
    <class kind="type">multio_api_configuration_mod::multio_configuration</class>
    <member kind="function">
      <type>integer(c_int) function, pointer</type>
      <name>multio_conf_move_failure_id</name>
      <anchorfile>namespacemultio__api__configuration__mod_adb38eec6d1528ae41fade7924b1a2fa1.html</anchorfile>
      <anchor>adb38eec6d1528ae41fade7924b1a2fa1</anchor>
      <arglist>(cc)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>type(c_ptr) function</type>
      <name>multio_configuration_c_ptr</name>
      <anchorfile>namespacemultio__api__configuration__mod_a2972d422d8530f2b2ff40ae128fe3f7f.html</anchorfile>
      <anchor>a2972d422d8530f2b2ff40ae128fe3f7f</anchor>
      <arglist>(cc)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_new_configuration</name>
      <anchorfile>namespacemultio__api__configuration__mod_a6cab6484fd7472c2d1e3a04d6b7e48be.html</anchorfile>
      <anchor>a6cab6484fd7472c2d1e3a04d6b7e48be</anchor>
      <arglist>(cc)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_new_configuration_from_filename</name>
      <anchorfile>namespacemultio__api__configuration__mod_a44c186e3ed8bde6441170e25a19b4697.html</anchorfile>
      <anchor>a44c186e3ed8bde6441170e25a19b4697</anchor>
      <arglist>(cc, file_name)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_delete_configuration</name>
      <anchorfile>namespacemultio__api__configuration__mod_a5dea51f6e0dc91d263697b7689a490fc.html</anchorfile>
      <anchor>a5dea51f6e0dc91d263697b7689a490fc</anchor>
      <arglist>(cc)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_conf_set_failure_handler</name>
      <anchorfile>namespacemultio__api__configuration__mod_a6f3af8ffec1aa7b890b818fb721dd827.html</anchorfile>
      <anchor>a6f3af8ffec1aa7b890b818fb721dd827</anchor>
      <arglist>(cc, handler, context)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_conf_set_path</name>
      <anchorfile>namespacemultio__api__configuration__mod_a7df080b003fdccccf771461b3583946a.html</anchorfile>
      <anchor>a7df080b003fdccccf771461b3583946a</anchor>
      <arglist>(cc, path)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_conf_mpi_allow_world_default_comm</name>
      <anchorfile>namespacemultio__api__configuration__mod_a970555b9abe60ae30498de1232a33fb3.html</anchorfile>
      <anchor>a970555b9abe60ae30498de1232a33fb3</anchor>
      <arglist>(cc, allow)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_conf_mpi_parent_comm</name>
      <anchorfile>namespacemultio__api__configuration__mod_ae28d4759a7ed6ad5672d08d6837842fa.html</anchorfile>
      <anchor>ae28d4759a7ed6ad5672d08d6837842fa</anchor>
      <arglist>(cc, parent_comm)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_conf_mpi_return_client_comm</name>
      <anchorfile>namespacemultio__api__configuration__mod_abaea42ca5b83c26835a5a809431957f2.html</anchorfile>
      <anchor>abaea42ca5b83c26835a5a809431957f2</anchor>
      <arglist>(cc, return_comm)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_conf_mpi_return_server_comm</name>
      <anchorfile>namespacemultio__api__configuration__mod_a2803989846b5fef66f23a0bb8cbee9c0.html</anchorfile>
      <anchor>a2803989846b5fef66f23a0bb8cbee9c0</anchor>
      <arglist>(cc, return_comm)</arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>multio_api_constants_mod</name>
    <filename>namespacemultio__api__constants__mod.html</filename>
    <member kind="variable">
      <type>integer, parameter, public</type>
      <name>multio_success</name>
      <anchorfile>namespacemultio__api__constants__mod_adc8dc0bff9f499822da99761fe76d03e.html</anchorfile>
      <anchor>adc8dc0bff9f499822da99761fe76d03e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter, public</type>
      <name>multio_error_eckit_exception</name>
      <anchorfile>namespacemultio__api__constants__mod_a485e4a9ab559ed2d5fa4ae901bf58734.html</anchorfile>
      <anchor>a485e4a9ab559ed2d5fa4ae901bf58734</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter, public</type>
      <name>multio_error_general_exception</name>
      <anchorfile>namespacemultio__api__constants__mod_aa67cb372ced6bff634f05fa5a411d92b.html</anchorfile>
      <anchor>aa67cb372ced6bff634f05fa5a411d92b</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter, public</type>
      <name>multio_error_unknown_exception</name>
      <anchorfile>namespacemultio__api__constants__mod_a49efea9795a2a133e67b6cb35e2f1f4e.html</anchorfile>
      <anchor>a49efea9795a2a133e67b6cb35e2f1f4e</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>multio_api_data_mod</name>
    <filename>namespacemultio__api__data__mod.html</filename>
    <class kind="type">multio_api_data_mod::multio_data</class>
    <member kind="function">
      <type>type(c_ptr) function</type>
      <name>multio_data_c_ptr</name>
      <anchorfile>namespacemultio__api__data__mod_a389eca5e3ea0e20693b4faa4760167be.html</anchorfile>
      <anchor>a389eca5e3ea0e20693b4faa4760167be</anchor>
      <arglist>(data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_new</name>
      <anchorfile>namespacemultio__api__data__mod_a841253af9dd200ed010360cd1fd89013.html</anchorfile>
      <anchor>a841253af9dd200ed010360cd1fd89013</anchor>
      <arglist>(data, handle, byte_size)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_delete</name>
      <anchorfile>namespacemultio__api__data__mod_a7247d908ce86b9a4a569a88750e90dc1.html</anchorfile>
      <anchor>a7247d908ce86b9a4a569a88750e90dc1</anchor>
      <arglist>(data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_zero</name>
      <anchorfile>namespacemultio__api__data__mod_afac11605d586eb49c90c194ab0cdcc9a.html</anchorfile>
      <anchor>afac11605d586eb49c90c194ab0cdcc9a</anchor>
      <arglist>(data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_resize</name>
      <anchorfile>namespacemultio__api__data__mod_a5b0f3805c42e6c2d3e67e596738e6085.html</anchorfile>
      <anchor>a5b0f3805c42e6c2d3e67e596738e6085</anchor>
      <arglist>(data, new_size)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_size</name>
      <anchorfile>namespacemultio__api__data__mod_a4bf2691c17489bcae64cd1735a81f162.html</anchorfile>
      <anchor>a4bf2691c17489bcae64cd1735a81f162</anchor>
      <arglist>(data, size)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_byte_size</name>
      <anchorfile>namespacemultio__api__data__mod_a7e15bbc32f815d16a0101640004657b8.html</anchorfile>
      <anchor>a7e15bbc32f815d16a0101640004657b8</anchor>
      <arglist>(data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_set_float_scalar</name>
      <anchorfile>namespacemultio__api__data__mod_a0f0e6c04c1b1878d958a09536e379e2e.html</anchorfile>
      <anchor>a0f0e6c04c1b1878d958a09536e379e2e</anchor>
      <arglist>(data, value, pos)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_set_float_chunk</name>
      <anchorfile>namespacemultio__api__data__mod_a9cf4b967eff018c0b1d6bde271fa09c6.html</anchorfile>
      <anchor>a9cf4b967eff018c0b1d6bde271fa09c6</anchor>
      <arglist>(data, value, pos, size)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_set_double_scalar</name>
      <anchorfile>namespacemultio__api__data__mod_a5be98c1f8c96378e9d1153f6a0c280f0.html</anchorfile>
      <anchor>a5be98c1f8c96378e9d1153f6a0c280f0</anchor>
      <arglist>(data, value, pos)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_data_set_double_chunk</name>
      <anchorfile>namespacemultio__api__data__mod_a1fa6abb8117c53d64e50b5251461a8c7.html</anchorfile>
      <anchor>a1fa6abb8117c53d64e50b5251461a8c7</anchor>
      <arglist>(data, value, pos, size)</arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>multio_api_error_handling_mod</name>
    <filename>namespacemultio__api__error__handling__mod.html</filename>
    <class kind="interface">multio_api_error_handling_mod::failure_handler_t</class>
    <class kind="type">multio_api_error_handling_mod::multio_failure_info</class>
    <class kind="type">multio_api_error_handling_mod::multio_fort_failure_info_list</class>
    <class kind="type">multio_api_error_handling_mod::multio_fort_failure_info_node</class>
    <member kind="function" protection="private">
      <type>type(c_funptr) function</type>
      <name>multio_fort_failure_wrapper_addr</name>
      <anchorfile>namespacemultio__api__error__handling__mod_aca30b9426edc77ab1473d076c5fc8aef.html</anchorfile>
      <anchor>aca30b9426edc77ab1473d076c5fc8aef</anchor>
      <arglist>(ffi)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>subroutine</type>
      <name>multio_fort_failure_call</name>
      <anchorfile>namespacemultio__api__error__handling__mod_a5eca0d52dac852d569bfe281bdb12081.html</anchorfile>
      <anchor>a5eca0d52dac852d569bfe281bdb12081</anchor>
      <arglist>(ffi, id, err, info)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>type(c_ptr) function</type>
      <name>multio_fort_failure_add</name>
      <anchorfile>namespacemultio__api__error__handling__mod_a1064b10ead27530d4dea323eefb825fe.html</anchorfile>
      <anchor>a1064b10ead27530d4dea323eefb825fe</anchor>
      <arglist>(ffi, handler_fn, context)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>subroutine</type>
      <name>multio_fort_failure_remove</name>
      <anchorfile>namespacemultio__api__error__handling__mod_a1f31b4c9679f3c5463c00fca0687c52a.html</anchorfile>
      <anchor>a1f31b4c9679f3c5463c00fca0687c52a</anchor>
      <arglist>(ffi, id)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>subroutine</type>
      <name>failure_handler_wrapper</name>
      <anchorfile>namespacemultio__api__error__handling__mod_a23eee5e788d3bd1bd59ca393af164c1d.html</anchorfile>
      <anchor>a23eee5e788d3bd1bd59ca393af164c1d</anchor>
      <arglist>(c_pidx, c_error, c_info)</arglist>
    </member>
    <member kind="variable">
      <type>type(multio_fort_failure_info_list), save, public</type>
      <name>failure_info_list</name>
      <anchorfile>namespacemultio__api__error__handling__mod_acc35a8cee743a4363b2da2dd21440b50.html</anchorfile>
      <anchor>acc35a8cee743a4363b2da2dd21440b50</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>multio_api_handle_mod</name>
    <filename>namespacemultio__api__handle__mod.html</filename>
    <class kind="type">multio_api_handle_mod::multio_handle</class>
    <member kind="function">
      <type>integer function</type>
      <name>multio_handle_flush</name>
      <anchorfile>namespacemultio__api__handle__mod_a41298c792cf86e0a3679321fe758f5b1.html</anchorfile>
      <anchor>a41298c792cf86e0a3679321fe758f5b1</anchor>
      <arglist>(handle, metadata)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_notify</name>
      <anchorfile>namespacemultio__api__handle__mod_a769f7c8df892a76453d31e1b14e69698.html</anchorfile>
      <anchor>a769f7c8df892a76453d31e1b14e69698</anchor>
      <arglist>(handle, metadata)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_domain</name>
      <anchorfile>namespacemultio__api__handle__mod_a997741f5bd292a104ce4fb2237093625.html</anchorfile>
      <anchor>a997741f5bd292a104ce4fb2237093625</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_mask_float_1d</name>
      <anchorfile>namespacemultio__api__handle__mod_a5a08d82c415200149b0f5562b84cb931.html</anchorfile>
      <anchor>a5a08d82c415200149b0f5562b84cb931</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_mask_float_2d</name>
      <anchorfile>namespacemultio__api__handle__mod_a8d0dbbc8338788e78b8ca2ce8f0cbf9d.html</anchorfile>
      <anchor>a8d0dbbc8338788e78b8ca2ce8f0cbf9d</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_mask_double_1d</name>
      <anchorfile>namespacemultio__api__handle__mod_ac9de24b8dae2bb0fbe3dd77acd09e9b9.html</anchorfile>
      <anchor>ac9de24b8dae2bb0fbe3dd77acd09e9b9</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_mask_double_2d</name>
      <anchorfile>namespacemultio__api__handle__mod_afd53da3b383a2d458f8d72efaf266a5f.html</anchorfile>
      <anchor>afd53da3b383a2d458f8d72efaf266a5f</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_field_float_1d</name>
      <anchorfile>namespacemultio__api__handle__mod_af7ed57fa7c6a6450656eec0b1b7678a1.html</anchorfile>
      <anchor>af7ed57fa7c6a6450656eec0b1b7678a1</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_field_float_2d</name>
      <anchorfile>namespacemultio__api__handle__mod_a076be536739b67187398d81524502af6.html</anchorfile>
      <anchor>a076be536739b67187398d81524502af6</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_field_double_1d</name>
      <anchorfile>namespacemultio__api__handle__mod_a2b5182b01ab31b82b566fa3c2ff4d40a.html</anchorfile>
      <anchor>a2b5182b01ab31b82b566fa3c2ff4d40a</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_field_double_2d</name>
      <anchorfile>namespacemultio__api__handle__mod_a2e1651749003025866e3c3929535d47d.html</anchorfile>
      <anchor>a2e1651749003025866e3c3929535d47d</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_write_field_buffer</name>
      <anchorfile>namespacemultio__api__handle__mod_a396aa9a98e8e11bb5090564b7192d216.html</anchorfile>
      <anchor>a396aa9a98e8e11bb5090564b7192d216</anchor>
      <arglist>(handle, metadata, data)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_handle_field_accepted</name>
      <anchorfile>namespacemultio__api__handle__mod_a4b69737ee4e7dd553371e00fa9ef16c8.html</anchorfile>
      <anchor>a4b69737ee4e7dd553371e00fa9ef16c8</anchor>
      <arglist>(handle, metadata, set_value)</arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>multio_api_metadata_mod</name>
    <filename>namespacemultio__api__metadata__mod.html</filename>
    <class kind="type">multio_api_metadata_mod::multio_metadata</class>
    <member kind="function">
      <type>type(c_ptr) function</type>
      <name>multio_metadata_c_ptr</name>
      <anchorfile>namespacemultio__api__metadata__mod_a971973cf98e879daa7a60697684c0460.html</anchorfile>
      <anchor>a971973cf98e879daa7a60697684c0460</anchor>
      <arglist>(metadata)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_new_metadata</name>
      <anchorfile>namespacemultio__api__metadata__mod_ae5b1b9bb1f65b760e1bdd6be8115097b.html</anchorfile>
      <anchor>ae5b1b9bb1f65b760e1bdd6be8115097b</anchor>
      <arglist>(metadata, handle)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_delete_metadata</name>
      <anchorfile>namespacemultio__api__metadata__mod_adb215b35fc2c5ff634693d3145e58bdc.html</anchorfile>
      <anchor>adb215b35fc2c5ff634693d3145e58bdc</anchor>
      <arglist>(metadata)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_metadata_set_string</name>
      <anchorfile>namespacemultio__api__metadata__mod_aa2d9a6447210740faf19234ccac916c0.html</anchorfile>
      <anchor>aa2d9a6447210740faf19234ccac916c0</anchor>
      <arglist>(metadata, key, value)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_metadata_set_int8</name>
      <anchorfile>namespacemultio__api__metadata__mod_a82ad5b3807f008e26d1513e34d99b99a.html</anchorfile>
      <anchor>a82ad5b3807f008e26d1513e34d99b99a</anchor>
      <arglist>(metadata, key, value)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_metadata_set_int16</name>
      <anchorfile>namespacemultio__api__metadata__mod_a3e6e39d39cddac5b86968cd9a2c3aea7.html</anchorfile>
      <anchor>a3e6e39d39cddac5b86968cd9a2c3aea7</anchor>
      <arglist>(metadata, key, value)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_metadata_set_int32</name>
      <anchorfile>namespacemultio__api__metadata__mod_aeabcb4a240124df96d498ed1f8d3a87b.html</anchorfile>
      <anchor>aeabcb4a240124df96d498ed1f8d3a87b</anchor>
      <arglist>(metadata, key, value)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_metadata_set_int64</name>
      <anchorfile>namespacemultio__api__metadata__mod_af0c8b3075db40897cec631a177245c21.html</anchorfile>
      <anchor>af0c8b3075db40897cec631a177245c21</anchor>
      <arglist>(metadata, key, value)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_metadata_set_real32</name>
      <anchorfile>namespacemultio__api__metadata__mod_ae52812a0c75e95e1e4e87069dc0c6c03.html</anchorfile>
      <anchor>ae52812a0c75e95e1e4e87069dc0c6c03</anchor>
      <arglist>(metadata, key, value)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_metadata_set_real64</name>
      <anchorfile>namespacemultio__api__metadata__mod_a0184c60d78f2d067d962bccc1f4dfb28.html</anchorfile>
      <anchor>a0184c60d78f2d067d962bccc1f4dfb28</anchor>
      <arglist>(metadata, key, value)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_metadata_set_fbool</name>
      <anchorfile>namespacemultio__api__metadata__mod_a66547e680758c94b3dcec96c2a7ced13.html</anchorfile>
      <anchor>a66547e680758c94b3dcec96c2a7ced13</anchor>
      <arglist>(metadata, key, value)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>integer function</type>
      <name>multio_metadata_set_cbool</name>
      <anchorfile>namespacemultio__api__metadata__mod_a00f6d3037b5c69b7f110c3f4d72ccaf2.html</anchorfile>
      <anchor>a00f6d3037b5c69b7f110c3f4d72ccaf2</anchor>
      <arglist>(metadata, key, value)</arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>multio_api_utils_mod</name>
    <filename>namespacemultio__api__utils__mod.html</filename>
    <member kind="function">
      <type>integer function, public</type>
      <name>multio_initialise</name>
      <anchorfile>namespacemultio__api__utils__mod_aca5fc0af011587af64e09292604027f9.html</anchorfile>
      <anchor>aca5fc0af011587af64e09292604027f9</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>integer function, public</type>
      <name>multio_start_server</name>
      <anchorfile>namespacemultio__api__utils__mod_ab9faa722cc754d73c10a4383526e79ea.html</anchorfile>
      <anchor>ab9faa722cc754d73c10a4383526e79ea</anchor>
      <arglist>(cc)</arglist>
    </member>
    <member kind="function" protection="private">
      <type>character(:) function, allocatable, target</type>
      <name>fortranise_cstr</name>
      <anchorfile>namespacemultio__api__utils__mod_a673954c16db544449efac701d2d0dbaf.html</anchorfile>
      <anchor>a673954c16db544449efac701d2d0dbaf</anchor>
      <arglist>(cstr)</arglist>
    </member>
    <member kind="function">
      <type>integer function, public</type>
      <name>multio_version</name>
      <anchorfile>namespacemultio__api__utils__mod_a268b6fb31ec6cea924d59a71baa22a67.html</anchorfile>
      <anchor>a268b6fb31ec6cea924d59a71baa22a67</anchor>
      <arglist>(version_str)</arglist>
    </member>
    <member kind="function">
      <type>integer function, public</type>
      <name>multio_vcs_version</name>
      <anchorfile>namespacemultio__api__utils__mod_a93516c58d996e482cbc442a21147caf1.html</anchorfile>
      <anchor>a93516c58d996e482cbc442a21147caf1</anchor>
      <arglist>(git_sha1)</arglist>
    </member>
    <member kind="function">
      <type>character(:) function, allocatable, target, public</type>
      <name>multio_error_string</name>
      <anchorfile>namespacemultio__api__utils__mod_a1b6859bc9d00ae073164000041931fe7.html</anchorfile>
      <anchor>a1b6859bc9d00ae073164000041931fe7</anchor>
      <arglist>(err, info)</arglist>
    </member>
  </compound>
</tagfile>
