0.4.0.0

  * Added `eitherMessageOrValue` to the `Extractors` module.

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
