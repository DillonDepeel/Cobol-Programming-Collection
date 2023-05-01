identification division.
       program-id. calculate.

       environment division.

       data division.
       working-storage section.
       01 calmemory      pic s9(9) comp-5 value 0.

       linkage section.
       01 calculator.
          05 arg1        pic s9(19)v9(19) comp-3.
          05 arg2        pic s9(19)v9(19) comp-3.
          05 result      pic s9(19)v9(19) comp-3.
          05 storage     pic s9(19)v9(19) comp-3.

       procedure division.
       exit program.

       entry "add" using calculator.
         move arg1 to result
         add  arg2 to result
         add  result to calmemory
         move calmemory to storage
         exit program.

       entry "subtract" using calculator.
         move arg1 to result
         subtract arg2 from result
         add  result to calmemory
         move calmemory to storage
         exit program.

      entry "multiply" using calculator.
         move arg1 to result
         multiply arg2 by result
         add  result to calmemory
         move calmemory to storage
         exit program.

      entry "divide" using calculator.
         move arg1 to result
         divide arg2 into result
         add  result to calmemory
         move calmemory to storage
         exit program.
