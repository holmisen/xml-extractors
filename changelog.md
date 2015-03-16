0.4.0.0

  * Added several functions to the `Extractors` module:

    * `eitherMessageOrValue` from the `ShowErr` module and some others.

	* Both conversion functions from `Extra`.

    This means less imports in many cases.

  * Some changes were made to the `Err` datatype:

    * `ErrExpect` was renamed to `ErrExpectContent`.

    * `ErrAttr` was renamed to `ErrExpectAttrib` and used for signalling a missing attribute.

    * `ErrAttribValue` was added to cater for erroneous attribute values.

    * Some field names have been given more descriptive names.

    The conversion/validation function given to `attribAs` does no
    longer need to return an `Err` value for errors, but only a string
    description of the expected value format.

0.3.0.0

  * Added `anyContent`
  * Added `choice`
