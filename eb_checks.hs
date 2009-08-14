

import Test.QuickCheck
import EditBuffer

instance Arbitrary Char where
  arbitrary = elements printableRange
    where printableRange = [' '..'~']

prop_insert_delete_inverse ch = 
  emptyBuffer == (deleteChar . moveLeft .insertChar ch) emptyBuffer 

prop_insert_char_advances n = 
  n >= 0 ==> n == x
               where (EditBuffer(x,_) contents) = (iterate (insertChar 'a') emptyBuffer) !! n

