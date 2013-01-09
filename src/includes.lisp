(in-package :cl-gi)

(define-foreign-library girepository
  (t (:default "libgirepository-1.0")))

(define-foreign-library glib
  (t (:default "libglib-2.0")))

(use-foreign-library girepository)

(use-foreign-library glib)


(defcenum gi-repository-error
  :typelib-not-found
  :namespace-mismatch
  :namespace-version-conflict
  :library-not-found)

(defctype g-type :int)

(defctype g-error :pointer)

;(defctype g-string-array :pointer)

(defctype gi-repository :pointer
  "Pointer that points to the GIRepository object")

(defctype gi-base-info :pointer
  "Basic pointer to GIBaseInfo from which all other info's should derive")

(defctype gi-typelib :pointer
  "GIR's typelib object")

#|

  girepository.h

|#

(defcfun "g_irepository_get_type" :int)

(defcfun "g_irepository_get_default" gi-repository)

(defcfun "g_irepository_prepend_search_path" :void (directory :string))
; (defcfun "g_irepository_get_search_path" g-s-list)
(defcfun "g_irepository_load_typelib" :string (repository gi-repository)
	 (typelib gi-typelib) (flags :int) (error :pointer))
(defcfun "g_irepository_is_registered" :boolean (repository gi-repository)
	 (namepsace :string) (version :string))
(defcfun "g_irepository_find_by_name" gi-base-info (repository gi-repository)
	 (name :string))
(defcfun "g_irepository_enumerate_versions" g-string-array (repository gi-repository) (namespace :string))
(defcfun "g_irepository_require" gi-typelib (repository gi-repository)
	 (namespace :string) (version :string) (flags :int) (error :pointer))
(defcfun "g_irepository_require_private" gi-typelib 
  (repository gi-repository) (directory :string) (namespace :string)
  (version :string) (flags :int) (error :pointer))
(defcfun "g_irepository_get_dependencies" g-string-array (repository gi-repository)
	 (namespace :string))
(defcfun "g_irepository_get_loaded_namespaces" g-string-array (repository gi-repository))
(defcfun "g_irepository_find_by_gtype" gi-base-info (repository gi-repository)
	 (gtype g-type))
(defcfun "g_irepository_get_n_infos" :int (repository gi-repository) (namespace :string))
(defcfun "g_irepository_get_info" gi-base-info (repository gi-repository)
	 (namespace :string) (index :int))
; (defcfun "g_irepository_find_by_error_domain" gi-enum-info (repository gi-repository) (domain g-quark))
(defcfun "g_irepository_get_typelib_path" :string (repository gi-repository)
	 (namespace :string))
(defcfun "g_irepository_get_shared_library" :string (repository gi-repository)
	 (namespace :string))
(defcfun "g_irepository_get_c_prefix" :string (repository gi-repository) (namespace :string))
(defcfun "g_irepository_get_version" :string (repository gi-repository) (namespace :string))
; (defcfun "g_irepository_get_option_group" g-option-group)
(defcfun "g_irepository_dump" :boolean (arg :string) (error g-error))

#|

  giarg.h
|#

(defcunion gi-argument
  (v_boolean :boolean)
  (v_int8    :int8)
  (v_uint8   :uint8)
  (v_int16   :int16)
  (v_uint16  :uint16)
  (v_int32   :int32)
  (v_uint32  :uint32)
  (v_int64   :int64)
  (v_uint64  :uint64)
  (v_float   :float)
  (v_double  :double)
  (v_short   :short)
  (v_ushort  :ushort)
  (v_int     :int)
  (v_uint    :uint)
  (v_long    :long)
  (v_ulong   :ulong)
  (v_ssize   :long)
  (v_size    :ulong)
  (v_string  :string)
  (v_pointer :pointer))

(defcfun "g_arg_info_get_type" gi-type-info (info gi-arg-info))

#|

   gibaseinfo.h

|#

(defcfun "g_base_info_gtype_get_type" g-type)
(defcfun "g_base_info_ref" gi-base-info (info gi-base-info))
(defcfun "g_base_info_unref" :void (info gi-base-info))
(defcfun "g_base_info_get_type" gi-info-type (info gi-base-info))
(defcfun "g_base_info_get_name" :string (info gi-base-info))
(defcfun "g_base_info_get_namespace" :string (info gi-base-info))
(defcfun "g_base_info_is_deprecated" :boolean (info gi-base-info))
(defcfun "g_base_info_get_attribute" :string (info gi-base-info) (name :string))

#|

   gitypeinfo.h

|#

(defctype gi-type-info gi-base-info)
(defctype gi-type-tag :int)

(defcfun "g_type_tag_to_string" :string (type gi-type-tag))
(defcfun "g_type_info_get_tag" gi-type-tag (info gi-type-info))

#|

  giconstantinfo.h

|#

(defctype gi-constant-info gi-base-info)

(defcfun "g_constant_info_get_value" :int
  (info gi-constant-info)
  (argument gi-argument))
(defcfun "g_constant_info_get_type" gi-type-info (info gi-constant-info))

#|

  gienuminfo.h

|#

(defctype gi-enum-info gi-base-info)
(defctype gi-value-info gi-base-info)

(defcfun "g_enum_info_get_n_values" :int (info gi-enum-info))
(defcfun "g_enum_info_get_value" gi-value-info (info gi-enum-info) (n :int))
(defcfun "g_value_info_get_value" :int64 (info gi-value-info))

#|

  gipropertyinfo.h

|#

(defctype gi-property-info gi-base-info)

(defcfun "g_property_info_get_type" gi-type-info (info gi-property-info))

#|

  gifunctioninfo.h

|#

(defctype gi-function-info gi-base-info)

(defcfun "g_function_info_get_property" gi-property-info (info gi-function-info))
