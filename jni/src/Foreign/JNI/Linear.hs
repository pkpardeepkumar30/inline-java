-- | Low-level bindings to the Java Native Interface (JNI).
--
-- Read the
-- <https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/jniTOC.html JNI spec>
-- for authoritative documentation as to what each of the functions in
-- this module does. The names of the bindings in this module were chosen to
-- match the names of the functions in the JNI spec.
--
-- All bindings in this module access the JNI via a thread-local variable of
-- type @JNIEnv *@. If the current OS thread has not yet been "attached" to the
-- JVM, it is attached implicitly upon the first call to one of these bindings
-- in the current thread.
--
-- The 'String' type in this module is the type of JNI strings. See
-- "Foreign.JNI.String".

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- XXX This file uses cpphs for preprocessing instead of the system's native
-- CPP, because the OS X has subtly different whitespace behaviour in the
-- presence of concatenation.

module Foreign.JNI.Linear where

import Control.Monad
import qualified Control.Monad.Linear as Linear
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Functor
import Data.Int
import Data.Word
import Foreign.C.Types
import qualified Foreign.JNI as JNI
import qualified Foreign.JNI.String as JNI
import Foreign.JNI.Types
import qualified System.IO.Linear as Linear
import qualified Unsafe.Linear as Unsafe
import qualified Unsafe.Linear.Extras as Unsafe
import Linear.Extras
import Prelude ((<$))
import Prelude.Linear hiding ((.))


-- | Create a new JVM, with the given arguments. /Can only be called once/. Best
-- practice: use 'withJVM' instead. Only useful for GHCi.
newJVM :: [ByteString] ->. Linear.IO JVM
newJVM = Linear.fromSystemIO . Unsafe.toLinear JNI.newJVM

-- | Deallocate a 'JVM' created using 'newJVM'.
destroyJVM :: JVM ->. Linear.IO ()
destroyJVM = Linear.fromSystemIO . Unsafe.toLinear JNI.destroyJVM

-- | Create a new JVM, with the given arguments. Destroy it once the given
-- action completes. /Can only be called once/. Best practice: use it to wrap
-- your @main@ function.
withJVM :: [ByteString] -> Linear.IO a ->. Linear.IO a
withJVM x y = Linear.fromSystemIO $
    Unsafe.toLinear2 JNI.withJVM x $ toSystemIO y

throw :: Coercible o (J a) => o ->. Linear.IO o
throw = Unsafe.toLinear $ \x -> Linear.fromSystemIO $ x <$ JNI.throw x

throwNew :: JClass -> JNI.String -> Linear.IO ()
throwNew jclass msg = Linear.fromSystemIO $ JNI.throwNew jclass msg

findClass :: ReferenceTypeName -> Linear.IO JClass
findClass name = Linear.fromSystemIO $ JNI.findClass name

newObject
  :: JClass
  -> MethodSignature
  -> [JValue]
  -> Linear.IO JObject
newObject jclass m args = Linear.fromSystemIO $ JNI.newObject jclass m args

getFieldID
  :: JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  -> Signature -- ^ JNI signature
  -> Linear.IO (Unrestricted JFieldID)
getFieldID jclass fieldname sig =
    Linear.fromSystemIO $ Unrestricted <$> JNI.getFieldID jclass fieldname sig

getStaticFieldID
  :: JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  -> Signature -- ^ JNI signature
  -> Linear.IO (Unrestricted JFieldID)
getStaticFieldID jclass fieldname sig =
    Linear.fromSystemIO $
      Unrestricted <$> JNI.getStaticFieldID jclass fieldname sig

#define GET_FIELD(name, hs_rettype) \
get/**/name/**/Field :: Coercible o (J a) => o ->. JFieldID -> Linear.IO (o, Unrestricted hs_rettype); \
get/**/name/**/Field = Unsafe.toLinear $ \obj field -> \
    Linear.fromSystemIO $ JNI.get/**/name/**/Field obj field <&> \
      \v -> (obj, Unrestricted v)

getObjectField :: Coercible o (J a) => o ->. JFieldID -> Linear.IO (o, JObject)
getObjectField = Unsafe.toLinear $ \obj field ->
    Linear.fromSystemIO $ JNI.getObjectField obj field <&>
      \v -> (obj, v)

GET_FIELD(Boolean, Word8)
GET_FIELD(Byte, CChar)
GET_FIELD(Char, Word16)
GET_FIELD(Short, Int16)
GET_FIELD(Int, Int32)
GET_FIELD(Long, Int64)
GET_FIELD(Float, Float)
GET_FIELD(Double, Double)

#define GET_STATIC_FIELD(name, hs_rettype) \
getStatic/**/name/**/Field :: JClass -> JFieldID -> Linear.IO (Unrestricted hs_rettype); \
getStatic/**/name/**/Field jclass field = \
    Linear.fromSystemIO $ Unrestricted <$> JNI.getStatic/**/name/**/Field jclass field

getStaticObjectField :: JClass -> JFieldID -> Linear.IO JObject
getStaticObjectField jclass field =
    Linear.fromSystemIO $ JNI.getStaticObjectField jclass field

GET_STATIC_FIELD(Boolean, Word8)
GET_STATIC_FIELD(Byte, CChar)
GET_STATIC_FIELD(Char, Word16)
GET_STATIC_FIELD(Short, Int16)
GET_STATIC_FIELD(Int, Int32)
GET_STATIC_FIELD(Long, Int64)
GET_STATIC_FIELD(Float, Float)
GET_STATIC_FIELD(Double, Double)

#define SET_FIELD(name, hs_fieldtype) \
set/**/name/**/Field :: Coercible o (J a) => o ->. JFieldID -> hs_fieldtype -> Linear.IO o; \
set/**/name/**/Field = Unsafe.toLinear $ \obj field v -> \
    Linear.fromSystemIO $ obj <$ JNI.set/**/name/**/Field obj field v

setObjectField
  :: Coercible o (J a)
  => o
  ->. JFieldID
  -> JObject
  ->. Linear.IO (o, JObject)
setObjectField =
    Unsafe.toLinear $ \obj field -> Unsafe.toLinear $ \v ->
      Linear.fromSystemIO $ (obj, v) <$ JNI.setObjectField obj field v

SET_FIELD(Boolean, Word8)
SET_FIELD(Byte, CChar)
SET_FIELD(Char, Word16)
SET_FIELD(Short, Int16)
SET_FIELD(Int, Int32)
SET_FIELD(Long, Int64)
SET_FIELD(Float, Float)
SET_FIELD(Double, Double)

#define SET_STATIC_FIELD(name, hs_fieldtype) \
setStatic/**/name/**/Field :: JClass -> JFieldID -> hs_fieldtype -> Linear.IO (); \
setStatic/**/name/**/Field jclass field v = \
    Linear.fromSystemIO $ JNI.setStatic/**/name/**/Field jclass field v

setStaticObjectField
  :: JClass
  -> JFieldID
  -> JObject
  ->. Linear.IO JObject
setStaticObjectField jclass field =
    Unsafe.toLinear $ \v ->
      Linear.fromSystemIO $ v <$ JNI.setStaticObjectField jclass field v

SET_STATIC_FIELD(Boolean, Word8)
SET_STATIC_FIELD(Byte, CChar)
SET_STATIC_FIELD(Char, Word16)
SET_STATIC_FIELD(Short, Int16)
SET_STATIC_FIELD(Int, Int32)
SET_STATIC_FIELD(Long, Int64)
SET_STATIC_FIELD(Float, Float)
SET_STATIC_FIELD(Double, Double)

getMethodID
  :: JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  -> MethodSignature -- ^ JNI signature
  -> Linear.IO JMethodID
getMethodID jclass methodname sig =
  Linear.fromSystemIO $ JNI.getMethodID jclass methodname sig

getStaticMethodID
  :: JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  -> MethodSignature -- ^ JNI signature
  -> Linear.IO JMethodID
getStaticMethodID jclass methodname sig =
  Linear.fromSystemIO $ JNI.getStaticMethodID jclass methodname sig

getObjectClass :: Coercible o (J ty) => o ->. Linear.IO (o, JClass)
getObjectClass = Unsafe.toLinear $ \o ->
    Linear.fromSystemIO $ (,) o <$> JNI.getObjectClass o

-- | Creates a global reference to the object referred to by
-- the given reference.
--
-- Arranges for a finalizer to call 'deleteGlobalRef' when the
-- global reference is no longer reachable on the Haskell side.
newGlobalRef :: Coercible o (J ty) => o ->. Linear.IO (o, Unrestricted o)
newGlobalRef = Unsafe.toLinear $ \o ->
    Linear.fromSystemIO $ JNI.newGlobalRef o <&>
      \g -> (o, Unrestricted g)


deleteGlobalRef :: Coercible o (J ty) => o -> Linear.IO ()
deleteGlobalRef o = Linear.fromSystemIO $ JNI.deleteGlobalRef o

-- | Like 'newGlobalRef' but it doesn't attach a finalizer to destroy
-- the reference when it is not longer reachable. Use
-- 'deleteGlobalRefNonFinalized' to destroy this reference.
newGlobalRefNonFinalized
  :: Coercible o (J ty) => o ->. Linear.IO (o, Unrestricted o)
newGlobalRefNonFinalized = Unsafe.toLinear $ \o ->
    Linear.fromSystemIO $ JNI.newGlobalRefNonFinalized o <&>
      \g -> (o, Unrestricted g)

-- | Like 'deleteGlobalRef' but it can be used only on references created with
-- 'newGlobalRefNonFinalized'.
deleteGlobalRefNonFinalized :: Coercible o (J ty) => o -> Linear.IO ()
deleteGlobalRefNonFinalized o = Linear.fromSystemIO $ JNI.deleteGlobalRef o


-- NB: Cannot add a finalizer to local references because it may
-- run in a thread where the reference is not valid.
newLocalRef :: Coercible o (J ty) => o ->. Linear.IO (o, o)
newLocalRef = Unsafe.toLinear $ \o ->
    Linear.fromSystemIO $ (,) o <$> JNI.newLocalRef o

deleteLocalRef :: Coercible o (J ty) => o ->. Linear.IO ()
deleteLocalRef = Unsafe.toLinear $ \o ->
    Linear.fromSystemIO $ JNI.deleteLocalRef o

{-
pushLocalFrame :: Int32 -> IO ()
pushLocalFrame (coerce -> capacity) = withJNIEnv $ \env ->
    -- We ignore the output as it is always 0 on success and throws an
    -- exception otherwise.
    throwIfException env $ void $
    [CU.block| jint {
      (*$(JNIEnv *env))->PushLocalFrame($(JNIEnv *env),
                                        $(jint capacity)); } |]

popLocalFrame :: Coercible o (J ty) => o -> IO o
popLocalFrame (coerce -> upcast -> obj) = withJNIEnv $ \env ->
    coerce <$> (objectFromPtr =<<)
    [CU.exp| jobject {
      (*$(JNIEnv *env))->PopLocalFrame($(JNIEnv *env),
                                       $fptr-ptr:(jobject obj)) } |]

-- Modern CPP does have ## for concatenating strings, but we use the hacky /**/
-- comment syntax for string concatenation. This is because GHC passes
-- the -traditional flag to the preprocessor by default, which turns off several
-- modern CPP features.

#define CALL_METHOD(name, hs_rettype, c_rettype) \
call/**/name/**/Method :: Coercible o (J a) => o -> JMethodID -> [JValue] -> IO hs_rettype; \
call/**/name/**/Method (coerce -> upcast -> obj) method args = withJNIEnv $ \env -> \
    throwIfException env $ \
    withJValues args $ \cargs -> \
    [C.exp| c_rettype { \
      (*$(JNIEnv *env))->Call/**/name/**/MethodA($(JNIEnv *env), \
                                                 $fptr-ptr:(jobject obj), \
                                                 $(jmethodID method), \
                                                 $(jvalue *cargs)) } |]

CALL_METHOD(Void, (), void)
callObjectMethod :: Coercible o (J a) => o -> JMethodID -> [JValue] -> IO JObject
callObjectMethod x y z =
    let CALL_METHOD(Object, (Ptr JObject), jobject)
    in objectFromPtr =<< callObjectMethod x y z
callBooleanMethod :: Coercible o (J a) => o -> JMethodID -> [JValue] -> IO Bool
callBooleanMethod x y z =
    let CALL_METHOD(Boolean, Word8, jboolean)
    in toEnum . fromIntegral <$> callBooleanMethod x y z
CALL_METHOD(Byte, CChar, jbyte)
CALL_METHOD(Char, Word16, jchar)
CALL_METHOD(Short, Int16, jshort)
CALL_METHOD(Int, Int32, jint)
CALL_METHOD(Long, Int64, jlong)
CALL_METHOD(Float, Float, jfloat)
CALL_METHOD(Double, Double, jdouble)

#define CALL_STATIC_METHOD(name, hs_rettype, c_rettype) \
callStatic/**/name/**/Method :: JClass -> JMethodID -> [JValue] -> IO hs_rettype; \
callStatic/**/name/**/Method cls method args = throwIfJNull cls $ \
    withJNIEnv $ \env -> \
    throwIfException env $ \
    withJValues args $ \cargs -> \
    [C.exp| c_rettype { \
      (*$(JNIEnv *env))->CallStatic/**/name/**/MethodA($(JNIEnv *env), \
                                                       $fptr-ptr:(jclass cls), \
                                                       $(jmethodID method), \
                                                       $(jvalue *cargs)) } |]

CALL_STATIC_METHOD(Void, (), void)
callStaticObjectMethod :: JClass -> JMethodID -> [JValue] -> IO JObject
callStaticObjectMethod x y z =
    let CALL_STATIC_METHOD(Object, (Ptr JObject), jobject)
    in objectFromPtr =<< callStaticObjectMethod x y z
callStaticBooleanMethod :: JClass -> JMethodID -> [JValue] -> IO Bool
callStaticBooleanMethod x y z =
    let CALL_STATIC_METHOD(Boolean, Word8, jboolean)
    in toEnum . fromIntegral <$> callStaticBooleanMethod x y z
CALL_STATIC_METHOD(Byte, CChar, jbyte)
CALL_STATIC_METHOD(Char, Word16, jchar)
CALL_STATIC_METHOD(Short, Int16, jshort)
CALL_STATIC_METHOD(Int, Int32, jint)
CALL_STATIC_METHOD(Long, Int64, jlong)
CALL_STATIC_METHOD(Float, Float, jfloat)
CALL_STATIC_METHOD(Double, Double, jdouble)

newObjectArray :: Int32 -> JClass -> IO JObjectArray
newObjectArray sz cls = throwIfJNull cls $ withJNIEnv $ \env ->
    throwIfException env $
    objectFromPtr =<<
    [CU.exp| jobjectArray {
      (*$(JNIEnv *env))->NewObjectArray($(JNIEnv *env),
                                        $(jsize sz),
                                        $fptr-ptr:(jclass cls),
                                        NULL) } |]

#define NEW_ARRAY(name, c_rettype) \
new/**/name/**/Array :: Int32 -> IO J/**/name/**/Array; \
new/**/name/**/Array sz = withJNIEnv $ \env -> \
    throwIfException env $ \
    objectFromPtr =<< \
    [CU.exp| c_rettype/**/Array { \
      (*$(JNIEnv *env))->New/**/name/**/Array($(JNIEnv *env), \
                                              $(jsize sz)) } |]

NEW_ARRAY(Boolean, jboolean)
NEW_ARRAY(Byte, jbyte)
NEW_ARRAY(Char, jchar)
NEW_ARRAY(Short, jshort)
NEW_ARRAY(Int, jint)
NEW_ARRAY(Long, jlong)
NEW_ARRAY(Float, jfloat)
NEW_ARRAY(Double, jdouble)

newString :: Ptr Word16 -> Int32 -> IO JString
newString ptr len = withJNIEnv $ \env ->
    throwIfException env $
    objectFromPtr =<<
    [CU.exp| jstring {
      (*$(JNIEnv *env))->NewString($(JNIEnv *env),
                                   $(jchar *ptr),
                                   $(jsize len)) } |]

getArrayLength :: Coercible o (JArray a) => o -> IO Int32
getArrayLength (coerce -> upcast -> array) = throwIfJNull array $
    withJNIEnv $ \env ->
    [C.exp| jsize {
      (*$(JNIEnv *env))->GetArrayLength($(JNIEnv *env),
                                        $fptr-ptr:(jarray array)) } |]

getStringLength :: JString -> IO Int32
getStringLength jstr = throwIfJNull jstr $ withJNIEnv $ \env ->
    [CU.exp| jsize {
      (*$(JNIEnv *env))->GetStringLength($(JNIEnv *env),
                                         $fptr-ptr:(jstring jstr)) } |]

#define GET_ARRAY_ELEMENTS(name, hs_rettype, c_rettype) \
get/**/name/**/ArrayElements :: J/**/name/**/Array -> IO (Ptr hs_rettype); \
get/**/name/**/ArrayElements (upcast -> array) = throwIfJNull array $ \
    withJNIEnv $ \env -> \
    throwIfNull $ \
    [CU.exp| c_rettype* { \
      (*$(JNIEnv *env))->Get/**/name/**/ArrayElements($(JNIEnv *env), \
                                                      $fptr-ptr:(jobject array), \
                                                      NULL) } |]

GET_ARRAY_ELEMENTS(Boolean, Word8, jboolean)
GET_ARRAY_ELEMENTS(Byte, CChar, jbyte)
GET_ARRAY_ELEMENTS(Char, Word16, jchar)
GET_ARRAY_ELEMENTS(Short, Int16, jshort)
GET_ARRAY_ELEMENTS(Int, Int32, jint)
GET_ARRAY_ELEMENTS(Long, Int64, jlong)
GET_ARRAY_ELEMENTS(Float, Float, jfloat)
GET_ARRAY_ELEMENTS(Double, Double, jdouble)

getStringChars :: JString -> IO (Ptr Word16)
getStringChars jstr = throwIfJNull jstr $ withJNIEnv $ \env ->
    throwIfNull $
    [CU.exp| const jchar* {
      (*$(JNIEnv *env))->GetStringChars($(JNIEnv *env),
                                        $fptr-ptr:(jstring jstr),
                                        NULL) } |]

#define SET_ARRAY_REGION(name, hs_argtype, c_argtype) \
set/**/name/**/ArrayRegion :: J/**/name/**/Array -> Int32 -> Int32 -> Ptr hs_argtype -> IO (); \
set/**/name/**/ArrayRegion array start len buf = throwIfJNull array $ \
    withJNIEnv $ \env -> \
    throwIfException env $ \
    [CU.exp| void { \
      (*$(JNIEnv *env))->Set/**/name/**/ArrayRegion($(JNIEnv *env), \
                                                    $fptr-ptr:(c_argtype/**/Array array), \
                                                    $(jsize start), \
                                                    $(jsize len), \
                                                    $(c_argtype *buf)) } |]

SET_ARRAY_REGION(Boolean, Word8, jboolean)
SET_ARRAY_REGION(Byte, CChar, jbyte)
SET_ARRAY_REGION(Char, Word16, jchar)
SET_ARRAY_REGION(Short, Int16, jshort)
SET_ARRAY_REGION(Int, Int32, jint)
SET_ARRAY_REGION(Long, Int64, jlong)
SET_ARRAY_REGION(Float, Float, jfloat)
SET_ARRAY_REGION(Double, Double, jdouble)

#define RELEASE_ARRAY_ELEMENTS(name, hs_argtype, c_argtype) \
release/**/name/**/ArrayElements :: J/**/name/**/Array -> Ptr hs_argtype -> IO (); \
release/**/name/**/ArrayElements (upcast -> array) xs = throwIfJNull array $ \
    withJNIEnv $ \env -> \
    [CU.exp| void { \
      (*$(JNIEnv *env))->Release/**/name/**/ArrayElements($(JNIEnv *env), \
                                                          $fptr-ptr:(jobject array), \
                                                          $(c_argtype *xs), \
                                                          JNI_ABORT) } |]

RELEASE_ARRAY_ELEMENTS(Boolean, Word8, jboolean)
RELEASE_ARRAY_ELEMENTS(Byte, CChar, jbyte)
RELEASE_ARRAY_ELEMENTS(Char, Word16, jchar)
RELEASE_ARRAY_ELEMENTS(Short, Int16, jshort)
RELEASE_ARRAY_ELEMENTS(Int, Int32, jint)
RELEASE_ARRAY_ELEMENTS(Long, Int64, jlong)
RELEASE_ARRAY_ELEMENTS(Float, Float, jfloat)
RELEASE_ARRAY_ELEMENTS(Double, Double, jdouble)

releaseStringChars :: JString -> Ptr Word16 -> IO ()
releaseStringChars jstr chars = throwIfJNull jstr $ withJNIEnv $ \env ->
    [CU.exp| void {
      (*$(JNIEnv *env))->ReleaseStringChars($(JNIEnv *env),
                                            $fptr-ptr:(jstring jstr),
                                            $(jchar *chars)) } |]

getObjectArrayElement
  :: forall a o.
     (IsReferenceType a, Coercible o (J a))
  => JArray a
  -> Int32
  -> IO o
getObjectArrayElement (arrayUpcast -> array) i = throwIfJNull array $
    withJNIEnv $ \env ->
    ( (coerce :: J a -> o)
    . (unsafeCast :: JObject -> J a)
    ) <$> (objectFromPtr =<<)
    [C.exp| jobject {
      (*$(JNIEnv *env))->GetObjectArrayElement($(JNIEnv *env),
                                               $fptr-ptr:(jarray array),
                                               $(jsize i)) } |]

setObjectArrayElement
  :: forall a o.
     (IsReferenceType a, Coercible o (J a))
  => JArray a
  -> Int32
  -> o
  -> IO ()
setObjectArrayElement (arrayUpcast -> array)
                      i
                      ((coerce :: o -> J a) -> upcast -> x) =
    throwIfJNull array $
    withJNIEnv $ \env ->
    [C.exp| void {
      (*$(JNIEnv *env))->SetObjectArrayElement($(JNIEnv *env),
                                               $fptr-ptr:(jobjectArray array),
                                               $(jsize i),
                                               $fptr-ptr:(jobject x)); } |]
-}
