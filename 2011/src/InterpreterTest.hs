import Test.HUnit

import Interpreter
import Actions
import Cards


-- let a = "test1" ~: 12 ~=? ( 1 + 1 )
-- runTestTT a
-- let b = test [ a , a, a, a]
-- runTestTT b

tests = test [ "init filter" ~: [] ~=? filterSlots defaultSlots,
               "world init" ~: do let world = defaultWorld
                                  [] @=? ( filterSlots $ proponent world )
                                  [] @=? ( filterSlots $ opponent world ),
               "0 zero" ~: do let move = Move RightApp Zero 0
                              let world = updateProponent defaultWorld move
                              [(0, Slot {field = Val 0, vitality = 10000})] @=? ( filterSlots $ proponent world )
                              [] @=? ( filterSlots $ opponent world ),
               "12 zero" ~: do let move = Move RightApp Zero 12
                               let world = updateProponent defaultWorld move
                               [(12, Slot {field = Val 0, vitality = 10000})] @=? ( filterSlots $ proponent world )
                               [] @=? ( filterSlots $ opponent world ),
               "0 Rdec R zero" ~:
                             do let move1 = Move RightApp Dec 0
                                let move2 = Move RightApp Zero 0
                                let world = updateProponent (updateProponent defaultWorld move1) move2
                                [] @=? ( filterSlots $ proponent world )
                                [(255,Slot {field = Func I, vitality = 9999})] @=? ( filterSlots $ opponent world ),
               "0 R Zero L dec" ~:
                             do let move1 = Move RightApp Zero 0
                                let move2 = Move LeftApp Dec 0
                                let world = updateProponent (updateProponent defaultWorld move1) move2
                                [] @=? ( filterSlots $ proponent world )
                                [(255,Slot {field = Func I, vitality = 9999})] @=? ( filterSlots $ opponent world )

             ]