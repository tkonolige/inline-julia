module Language.Julia.Inline.Marshal where

[ (''Word64, jlBoxWord64)
, (''Word32, jlBoxWord32)
, (''Word16, jlBoxWord16)
, (''Word8, jlBoxWord8)
, (''Int64, jlBoxInt64)
, (''Int32, jlBoxInt32)
, (''Int16, jlBoxInt16)
, (''Int8, jlBoxInt8)
, (''Float, jlBoxFloat)
, (''Double, jlBoxDouble)
]

[ ("Int64", jlUnboxInt64)
, ("Int32", jlUnboxInt32)
, ("Int16", jlUnboxInt16)
, ("Int8", jlUnboxInt8)
, ("UInt64", jlUnboxWord64)
, ("UInt32", jlUnboxWord32)
, ("UInt16", jlUnboxWord16)
, ("UInt8", jlUnboxWord8)
, ("Float", jlUnboxFloat)
, ("Double", jlUnboxDouble)
]
