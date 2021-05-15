- [1. Data types](#1-data-types)
  - [1.1. Integers](#11-integers)
  - [1.2. Decimals](#12-decimals)
  - [1.3. Texts](#13-texts)
  - [1.4. Logical](#14-logical)
  - [1.5. Sets, enumerations and subtypes](#15-sets-enumerations-and-subtypes)
    - [1.5.1. Enumerations *(`enum`)*](#151-enumerations-enum)
    - [1.5.2. SubRanges](#152-subranges)
    - [1.5.3. Sets](#153-sets)
      - [1.5.3.1. Including and excluding set values](#1531-including-and-excluding-set-values)
      - [1.5.3.2. Set operators](#1532-set-operators)
  - [1.6. Compound data types](#16-compound-data-types)
    - [1.6.1. Arrays](#161-arrays)
    - [1.6.2. Records](#162-records)
- [2. Operators](#2-operators)
  - [2.1. Binary Arithmetic Operators](#21-binary-arithmetic-operators)
  - [2.2. Unary arithmetic operators:](#22-unary-arithmetic-operators)
  - [2.3. Boolean Operators:](#23-boolean-operators)
  - [2.4. Logical (Bitwise) Operators:](#24-logical-bitwise-operators)
  - [2.5. String Operators:](#25-string-operators)
  - [2.6. Relational Operators:](#26-relational-operators)
  - [2.7. Precedence of operators](#27-precedence-of-operators)
- [3. Programming logic](#3-programming-logic)
  - [3.1. If then else](#31-if-then-else)
  - [3.2. Case statements](#32-case-statements)
    - [3.2.1. A simple numerical case statement](#321-a-simple-numerical-case-statement)
    - [3.2.2. Using the otherwise clause](#322-using-the-otherwise-clause)
    - [3.2.3. Using enumeration case values](#323-using-enumeration-case-values)
  - [3.3. Loops](#33-loops)
    - [3.3.1. For](#331-for)
    - [3.3.2. Repeat](#332-repeat)
    - [3.3.3. While](#333-while)
  - [3.4. Subroutines](#34-subroutines)
    - [3.4.1. A procedure without parameters](#341-a-procedure-without-parameters)
    - [3.4.2. A procedure with parameters](#342-a-procedure-with-parameters)
    - [3.4.3. A function without parameters](#343-a-function-without-parameters)
    - [3.4.4. A function with parameters](#344-a-function-with-parameters)
    - [3.4.5. Passing data by reference](#345-passing-data-by-reference)
    - [3.4.6. Output only parameters](#346-output-only-parameters)
    - [3.4.7. Constant value parameters](#347-constant-value-parameters)
    - [3.4.8. Same routine, different parameters](#348-same-routine-different-parameters)
- [4. Exception handling](#4-exception-handling)
  - [4.1. Try, except](#41-try-except)
  - [4.2. Raising exceptions](#42-raising-exceptions)
- [5. Dates and times](#5-dates-and-times)
  - [5.1. TDateTime](#51-tdatetime)
  - [5.2. Short and long month names](#52-short-and-long-month-names)
  - [5.3. Short and long day names](#53-short-and-long-day-names)
  - [5.4. Date and time calculations](#54-date-and-time-calculations)
  - [5.5. Displaying date and time values](#55-displaying-date-and-time-values)
    - [5.5.1. Displaying date and time values](#551-displaying-date-and-time-values)
    - [5.5.2. Formatting control variables](#552-formatting-control-variables)
- [6. Files](#6-files)
  - [6.1. Accessing files](#61-accessing-files)
  - [6.2. Reading and writing to text files](#62-reading-and-writing-to-text-files)
    - [6.2.1. Example code : Reading one character at a time from a text file line](#621-example-code--reading-one-character-at-a-time-from-a-text-file-line)
  - [6.3. Reading and writing to typed binary files](#63-reading-and-writing-to-typed-binary-files)
  - [6.4. Reading and writing to pure binary files](#64-reading-and-writing-to-pure-binary-files)
  - [6.5. Other file processing mechanisms](#65-other-file-processing-mechanisms)
  - [6.6. Getting information about files and directories](#66-getting-information-about-files-and-directories)
  - [6.7. Using TStringList to read and write text files](#67-using-tstringlist-to-read-and-write-text-files)
  - [6.8. Extract file path](#68-extract-file-path)
  - [6.9. ParamStr](#69-paramstr)
- [7. Conversions](#7-conversions)
  - [7.1. Integer to string](#71-integer-to-string)
  - [7.2. Float to string](#72-float-to-string)
  - [7.3. String to integer](#73-string-to-integer)
  - [7.4. Tryparse string to integer](#74-tryparse-string-to-integer)
- [8. Format](#8-format)
- [9. Class](#9-class)
- [10. UI elements](#10-ui-elements)
  - [10.1. Label](#101-label)
  - [10.2. Dialog](#102-dialog)
    - [10.2.1. Confirmation dialog](#1021-confirmation-dialog)
    - [10.2.2. Custom dialog with custom button selection](#1022-custom-dialog-with-custom-button-selection)
  - [10.3. Picture](#103-picture)
  - [10.4. Timer](#104-timer)
  - [10.5. Toolbar](#105-toolbar)
  - [10.6. Form](#106-form)
    - [10.6.1. Create another form](#1061-create-another-form)
  - [10.7. ListBox](#107-listbox)
  - [10.8. Drag and drop](#108-drag-and-drop)
    - [10.8.1. ListBox](#1081-listbox)
  - [10.9. StringGrid](#109-stringgrid)
  - [Canvas](#canvas)
    - [Draw line](#draw-line)
    - [Draw ellipse](#draw-ellipse)
- [11. Good to know](#11-good-to-know)
  - [11.1. Generate random character](#111-generate-random-character)
  - [11.2. Textbox `Edit` when press enter](#112-textbox-edit-when-press-enter)
- [12. Errors](#12-errors)

# 1. Data types

## 1.1. Integers

```pas
var
    Int1 : Byte;     //                        0 to 255
    Int2 : ShortInt; //                     -127 to 127
    Int3 : Word;     //                        0 to 65,535
    Int4 : SmallInt; //                  -32,768 to 32,767
    Int5 : LongWord; //                        0 to 4,294,967,295
    Int6 : Cardinal; //                        0 to 4,294,967,295
    Int7 : LongInt;  //           -2,147,483,648 to 2,147,483,647
    Int8 : Integer;  //           -2,147,483,648 to 2,147,483,647
    Int9 : Int64;    // -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807
```

## 1.2. Decimals
```pas
var
    Dec1 : Single;   //  7  significant digits, exponent   -38 to +38
    Dec2 : Currency; // 50+ significant digits, fixed 4 decimal places
    Dec3 : Double;   // 15  significant digits, exponent  -308 to +308
    Dec4 : Extended; // 19  significant digits, exponent -4932 to +4932
```

## 1.3. Texts

```pas
var
   Str1 : Char;        // Holds a single character, small alphabet
   Str2 : WideChar;    // Holds a single character, International alphabet
   Str3 : AnsiChar;    // Holds a single character, small alphabet
   Str4 : ShortString; // Holds a string of up to 255 Char's
   Str5 : String;      // Holds strings of Char's of any size desired
   Str6 : AnsiString;  // Holds strings of AnsiChar's any size desired
   Str7 : WideString;  // Holds strings of WideChar's of any size desired
```

## 1.4. Logical

```pas
var
   Log1 : Boolean;     // Can be 'True' or 'False'
```

## 1.5. Sets, enumerations and subtypes

```pas
type
   TSuit = (Hearts, Diamonds, Clubs, Spades);    // Defines the enumeration
 var
   suit : TSuit;                                 // An enumeration variable
```

Sets are often confused with enumerations.

An **enumeration variable** can have **only one of the enumerated values**.

A **set** can have **none**, **1**, **some**, or **all of the set values**.

```pas
type
   TWeek = Set of 1..7;      // Set comprising the days of the week, by number
 var
   week : TWeek;
 begin
   week := [1,2,3,4,5];      // Switch on the first 5 days of the week
 end;
```

### 1.5.1. Enumerations *(`enum`)*

```pas
type
   TSuit = (Hearts, Diamonds, Clubs, Spades);   // Defines enumeration range
 var
   suit : TSuit;                                // Defines enumeration variable
 begin
   suit := Clubs;                               // Set to one of the values
 end;
```

The TSuit type definition creates a new Delphi data type that we can use as a type for any new variable in our program.

```pas
 type
   TDay = (Mon=1, Tue, Wed, Thu, Fri, Sat, Sun);   // Enumeration values
 var
   today   : TDay;
   weekend : Boolean;
 begin
   today := Wed;          // Set today to be Wednesday
   
   if today > Fri         // Ask if it is a weekend day
   then weekend := true
   else weekend := false;
 end;
```

`today` is set to `Wed` which has ordinal value = `3`
`weekend` is set to `false` since `Wed` (`3`) `<=` `Fri` (`5`)

```pas
type
   TDay = (Mon=1, Tue, Wed, Thu, Fri, Sat, Sun);   // Enumeration values
 var
   day : TDay;          // Enumeration variable
 begin
   for day := Mon to Fri do
   begin
     // day has each of the values Mon to Fri ( 1 to 5) in 5 iterations
     // of this loop, allowing you to whatever you want.
   end;
 end;
```

> **Warning** : each of the values in an enumeration must be unique in a program. This restriction allows you to assign an enumeration value without having to qualify the type it is defined in.

### 1.5.2. SubRanges

```pas
type
   TSmallNum = 0..9;
 var
   smallNum : TSmallNum;
 begin
   smallNum := 5;    // Allowed
   smallNum := 10;   // Not allowed
   smallNum := -1;   // Not allowed
 end;
```

> Delphi will not compile code that has assignments outside of the given range.

Subranges of characters:

```pas
type
   TUpper = 'A'..'Z';
   TLower = 'a'..'z';
   TDigit = '0'..'9';
 var
   upper : TUpper;
   lower : TLower;
   digit : TDigit;
 begin
   upper := 'G';   // Allowed
   lower := 'g';   // Allowed
   digit := '7';   // Allowed
 
   upper := 'g';   // Not allowed
   lower := '7';   // Not allowed
   digit := 4;     // Not allowed
 end;
```

Subrange of enumerations:

```pas
type
    TDay = (Mon=1, Tue, Wed, Thu, Fri, Sat, Sun);   // Enumeration values
    TWeekDays = Mon..Fri;                           // Enumeration subranges
    TWeekend  = Sat..Sun;
```

### 1.5.3. Sets

```pas
type
   TDigits = set of '1'..'9';       // Set of numeric digit characters
 var
   digits : TDigits;                // Set variable
   myChar : char;
 begin
   // At the start, digits has all set values switched off
   // So let us switch some on. Notice how we can switch on single
   // values, and ranges, all in the one assignment:
   digits := ['2', '4'..'7'];
 
   // Now we can test to see what we have set on:
   for myChar := '1' to '9' do
     if myChar In digits
     then ShowMessageFmt('''%s'' is in digits',[myChar])
     else ShowMessageFmt('''%s'' is not in digits',[myChar])
 end;
```

The `In` operator tests to see if a set contains a value.

The data shown is as follows:

```
'1' is not in digits
'2' is in digits
'3' is not in digits
'4' is in digits
'5' is in digits
'6' is in digits
'7' is in digits
'8' is not in digits
'9' is not in digits
```

#### 1.5.3.1. Including and excluding set values

`Include` *(switch on)* or `exclude` *(switch off)* individual values without affecting other values.

```pas
type
   // We define a set by type - bytes have the range : 0 to 255
   TNums = set of Byte;
 var
   nums : TNums;
 begin
   nums := [20..50];     // Switch on a range of 31 values
   Include(nums, 12);    // Switch on an additional value : 12
   Exclude(nums, 35);    // Switch off a value : 35
 end;
```
	
`nums` now has the following values set : `12 , 20..34 , 36..50`

#### 1.5.3.2. Set operators

| operator | description                      |
| :------: | :------------------------------- |
|   `+`    | union of two sets                |
|   `*`    | intersection of two sets         |
|   `-`    | difference of two sets           |
|   `=`    | tests for identical sets         |
|   `<>`   | tests for non-identical sets     |
|   `>=`   | is one set a subset of another   |
|   `<=`   | is one set a superset of another |

```pas
type
   TNums = set of 1..9;
 var
   nums1, nums2, nums3, nums4, nums5, nums6 : TNums;
 begin
   nums1 := [1,2,3];
   nums2 := [1,2,4];
   nums3 := [1,2,3,4,5,6,7,8,9];
 
   nums4 := nums1 + nums2;    // nums4 now [1,2,3,4]
   nums5 := nums1 * nums2;    // nums5 now [1,2]
   nums6 := nums1 - nums2;    // nums6 now [3]
 
   // Test for equality
   if nums1 = nums2
   then ShowMessage('nums1 =  nums2')
   else ShowMessage('nums1 <> nums2');
 
   // Test for inequality
   if nums1 <> nums3
   then ShowMessage('nums1 <> nums3')
   else ShowMessage('nums1 =  nums3');
 
   // Is nums1 a subset of nums3?
   if nums1 <= nums3
   then ShowMessage('nums1 is a subset of nums3')
   else ShowMessage('nums1 is not a subset of nums3');
 
   // Is nums1 a superset of nums3?
   if nums1 >= nums3
   then ShowMessage('nums1 is a superset of nums3')
   else ShowMessage('nums1 is not a superset of nums3');
 end;
```

**Output**:

```
nums1 <> nums2
nums1 <> nums3
nums1 is a subset of nums3
nums1 is not a superset of nums3
```

## 1.6. Compound data types

### 1.6.1. Arrays

```pas
var
   Suits : array[1..4] of String;    // A list of 4 playing card suit names
 
 begin
   Suits[1] := 'Hearts';    // Assigning to array index 1
   Suits[2] := 'Diamonds';  // Assigning to array index 2
   Suits[3] := 'Clubs';     // Assigning to array index 3
   Suits[4] := 'Spades';    // Assigning to array index 4
 end;
```

The array defined above has indexes 1 to 4 (1..4). The two dots indicate a range.

### 1.6.2. Records

*Like a `class` or a `struct`, but not because there are other data types for object oriented things*

```pas
type
   TCustomer=Record
     firstName : string[20];
     lastName  : string[20];
     age       : byte;
   end;
```

```pas
var
  customer : TCustomer;            // Our customer variable
begin
  customer.firstName := 'Fred';    // Assigning to the customer record
  customer.lastName  := 'Bloggs';
  customer.age       := 55;
end;
```

**Output**:

```
customer.firstName is now set to 'Fred'
customer.lastName  is now set to 'Bloggs'
customer.age       is now set to 55
```

Or

```pas
var c : Card;

with c do
begin
  cType := i;
  cNumber := j;
end;
```

# 2. Operators

## 2.1. Binary Arithmetic Operators

| Operator | Operation        | Operand Types | Result Type   | Example            |
| :------: | ---------------- | ------------- | ------------- | ------------------ |
|   `+`    | addition         | integer, real | integer, real | X + Y              |
|   `-`    | subtraction      | integer, real | integer, real | Result -1          |
|   `*`    | multiplication   | integer, real | integer, real | P * InterestRate   |
|   `/`    | real division    | integer, real | real          | X / 2              |
|  `div`   | integer division | integer       | integer       | Total div UnitSize |
|  `mod`   | remainder        | integer       | integer       | Y mod 6            |

## 2.2. Unary arithmetic operators:

| Operator | Operation     | Operand Types | Result Type   | Example |
| :------: | ------------- | ------------- | ------------- | ------- |
|   `+`    | sign identity | integer, real | integer, real | +7      |
|   `-`    | sign negation | integer, real | integer, real | -X      |

The following rules apply to arithmetic operators:
- The value of `x / y` is of type `Extended`, regardless of the types of `x` and `y`. For other arithmetic operators, the result is of type `Extended` whenever at least one operand is a `real`, otherwise, the result is of type `Int64` when at least one operand is of type `Int64`, otherwise, the result is of type `Integer`. If an operand's type is a subrange of an integer type, it is treated as if it were of the integer type.
- The value of `x div y` is the value of `x / y` rounded in the direction of zero to the nearest integer.
- The `mod` operator returns the remainder obtained by dividing its operands. In other words,
  - `x mod y = x - (x div y) * y`
- A run-time error occurs when `y` is **zero** in an expression of the form
  - `x / y`
  - `x div y`
  - `x mod y`

## 2.3. Boolean Operators:

| Operator | Operation             | Operand Types | Result Type | Example             |
| :------: | --------------------- | ------------- | ----------- | ------------------- |
|  `not`   | negation              | Boolean       | Boolean     | not (C in MySet)    |
|  `and`   | conjunction           | Boolean       | Boolean     | Done and (Total >0) |
|   `or`   | disjunction           | Boolean       | Boolean     | A or B              |
|  `xor`   | exclusive disjunction | Boolean       | Boolean     | A xor B             |

## 2.4. Logical (Bitwise) Operators:

| Operator | Operation           | Operand Types | Result Type | Example |
| :------: | ------------------- | ------------- | ----------- | ------- |
|  `not`   | bitwise negation    | integer       | integer     | not X   |
|  `and`   | bitwise and         | integer       | integer     | X and Y |
|   `or`   | bitwise or          | integer       | integer     | X or Y  |
|  `xor`   | bitwise xor         | integer       | integer     | X xor Y |
|  `shl`   | bitwise shift left  | integer       | integer     | X shl 2 |
|  `shr`   | bitwise shift right | integer       | integer     | Y shr I |

The following rules apply to bitwise operators:

- The result of a `not` operation is of the same type as the operand.
- If the operands of an `and`, `or`, or `xor` operation are both integers, the result is of the predefined integer type with the smallest range that includes all possible values of both types.
- The operations `x shl y` and `x shr y` shift the value of x to the left or right by y bits, which (if x is an unsigned integer) is equivalent to multiplying or dividing x by 2^y; the result is of the same type as x. For example, if N stores the value 01101 (decimal 13), then N shl 1 returns 11010 (decimal 26). Note that the value of y is interpreted modulo the size of the type of x. Thus for example, if x is an integer, x shl 40 is interpreted as x shl 8 because an integer is 32 bits and 40 mod 32 is 8.

## 2.5. String Operators:

| Operator | Operation     | Operand Types                    | Result Type | Example |
| :------: | ------------- | -------------------------------- | ----------- | ------- |
|   `+`    | concatenation | string, packed string, character | string      | S + '.' |

## 2.6. Relational Operators:

| Operator | Operation                | Operand Types                                                    | Result Type | Example  |
| :------: | ------------------------ | ---------------------------------------------------------------- | ----------- | -------- |
|   `=`    | equality                 | simple, class, class reference, interface, string, packed string | Boolean     | I = Max  |
|   `<>`   | inequality               | simple, class, class reference, interface, string, packed string | Boolean     | X <> Y   |
|   `<`    | less-than                | simple, string, packed string, PChar                             | Boolean     | X < Y    |
|   `>`    | greater-than             | simple, string, packed string, PChar                             | Boolean     | Len > 0  |
|   `<=`   | less-than-or-equal-to    | simple, string, packed string, PChar                             | Boolean     | Cnt <= I |
|   `>=`   | greater-than-or-equal-to | simple, string, packed string, PChar                             | Boolean     | I >= 1   |

## 2.7. Precedence of operators

1. `@ not`
2. `* div mod and shl shr as`
3. `+ - or xor`
4. `= <> < > <= >= in is`  


# 3. Programming logic

## 3.1. If then else

```pas
var
   number : Integer;
   text   : String;
 begin
   number := Sqr(17);          // Calculate the square of 17
   if number > 400                  
   then text := '17 squared > 400'    // Action when if condition is true
   else text := '17 squared <= 400';  // Action when if condition is false
 end;
```

**Output**:

```
text is set to : '17 squared <= 400'
```

**Multiple**:

```pas
if (condition1) And (condition2)   // Both conditions must be satisfied
   then
     begin
       statement1;
       statement2;
       ...
     end              // Notice no terminating ';' - still part of 'if'
   else
     begin
       statement3;
       statement4;
       ...
     end;
```

**Nested**:

```pas
if condition1
   then statement1
   else if condition2
        then statement2
        else statement3;
```

## 3.2. Case statements

### 3.2.1. A simple numerical case statement

```pas
var
   i : Integer;
 begin
   i := RandomRange(15,20);  // Generate a random number from 15 to 20
   Case i of
     15 : ShowMessage('Random number was fifteen');
     16 : ShowMessage('Random number was sixteen');
     17 : ShowMessage('Random number was seventeen');
     18 : ShowMessage('Random number was eighteen');
     19 : ShowMessage('Random number was nineteen');
     20 : ShowMessage('Random number was twenty');
   end;
 end;
```

**Output**:

```
Random number was fifteen
```

The `RandomRange` routine generates a random number between two given values. However, each time you run the program, it will always start with the same pseudo random value.

### 3.2.2. Using the otherwise clause

```pas
var
   i : Integer;
 begin
   i := RandomRange(10,20);  // Generate a random number from 10 to 20
   Case i of
     15 : ShowMessage('Random number was fifteen');
     16 : ShowMessage('Random number was sixteen');
     17 : ShowMessage('Random number was seventeen');
     18 : ShowMessage('Random number was eighteen');
     19 : ShowMessage('Random number was nineteen');
     20 : ShowMessage('Random number was twenty');
   else
     ShowMessageFmt('Unexpected number : %d',[i]);
   end;
 end;
```

**Output**:

```
Unexpected number : 10
```

### 3.2.3. Using enumeration case values

```pas
type
   TCar = (Nissan, Ford, Rover, Jaguar);    // An enumeration type
 var     
   car : TCar;                              // An enumeration variable
 begin
   car := Rover;                            // Set this variable
   case car of
     Nissan : ShowMessage('We have a Nissan car');
     Ford   : ShowMessage('We have a Ford car');
     Rover  : ShowMessage('We have a Rover car');
     Jaguar : ShowMessage('We have a Jaguar car');
   end;
 end;
```

**Output**:

```
We have a Rover car
```

## 3.3. Loops

### 3.3.1. For

```pas
var
  count : Integer;
begin
  For count := 1 to 5 do
    ShowMessageFmt('Count is now %d',[count]);
end;
```

**Output**:

```
Count is now 1
Count is now 2
Count is now 3
Count is now 4
Count is now 5
```

```pas
type
   TWeekDay = (Monday=1, Tuesday, Wednesday, Thursday, Friday);
var
  weekday : TWeekDay;
  hours   : array[TWeekDay] of byte;
begin
  // Set up the hours every day to zero
  for weekDay := Monday to Friday do
    hours[weekDay] := 0;

  // Add an hour of overtime to the working hours on Tuesday to Thursday
  for weekDay := Tuesday to Thursday do
    Inc(hours[weekDay]);
end;
```

**Output**:

```
hours[Monday]    = 00
hours[Tuesday]   = 10
hours[Wednesday] = 10
hours[Thursday]  = 1
hours[Friday]    = 0
```

> `Inc`: increment the hours

```pas
var
  letter : Char;
begin
  for letter := 'G' downto 'A' do
    ShowMessage('Letter = '+letter)
end;
```

**Output**:

```
Letter = G
Letter = F
Letter = E
Letter = D
Letter = C
Letter = B
Letter = A
```

### 3.3.2. Repeat

```pas
var
  exit : Boolean;          // Our exit condition flag
  i : Integer;
begin
  i := 1;
  exit := False;           // do not exit until we are ready
  repeat
    Inc(i);                // Increment a count
    if Sqr(i) > 99
    then exit := true;     // Exit if the square of our number exceeds 99
  until exit;              // Shorthand for 'until exit := true'
end;
```

**Output**:

```
Upon exit, i will be 10 (since Sqr(10) > 99)
```

```pas
var
  i : Integer;
begin
  i := 1;
  repeat
    Inc(i);                // Increment a count
  until (Sqr(i) > 99) or (Sqrt(i) > 2.5);
end;
```

**Output**:

```
Upon exit, i will be 7 (since Sqrt(7) > 2.5)
```

### 3.3.3. While

```pas
var
  i : Integer;
begin
  i := 1;
  while (Sqr(i) <= 99) and (Sqrt(i) <= 2.5) do
    Inc(i);                // Increment a count
end;
```

**Output**:

```
Upon exit, i will be 7 (since Sqrt(7) > 2.5)
```

## 3.4. Subroutines

### 3.4.1. A procedure without parameters

```pas
procedure ShowTime;                    // A procedure with no parameters
begin
  // Display the current date and time
  ShowMessage('Date and time is '+DateTimeToStr(Now));
end;

// Let us call this procedure
ShowTime;
```

**Output**:

```
Date and time is 12/12/2002 15:30:45
```

### 3.4.2. A procedure with parameters

```pas
procedure ShowTime(dateTime : TDateTime);  // With parameters
begin
  // Display the date and time passed to the routine
  ShowMessage('Date and time is '+DateTimeToStr(dateTime));
end;

// Let us call this procedure
ShowTime(Yesterday);
```
	
**Output**:

```
Date and time is 11/12/2002
```

### 3.4.3. A function without parameters

```pas	
function RandomChar : char;
var
  i : integer;
begin
  // Get a random number from 65 to 90
  // (These numbers equate to characters 'A' to 'Z'
  i := RandomRange(65, 90);
  
  // Return this value as a char type in the return variable, Result
  Result := Chr(i);
end;

// Let us call this function
ShowMessage('Char chosen is : '+RandomChar);
```

**Output**:

```
Char chosen is : A
```

### 3.4.4. A function with parameters
	
```pas
function Average(a, b, c : Extended) : Extended;
begin
  // return the average of the 3 passed numbers
  Result := Mean(a, b, c);
end;

// Let us call this function
ShowMessageFmt('Average of 2, 13 and 56 = %f',[Average(2,13,56)]);
```
	
**Output**:

```
Average of 2, 13 and 56 = 23.67
```

---

```pas
// Full Unit code.
// -----------------------------------------------------------
// You must store this code in a unit called Unit1 with a form
// called Form1 that has an OnCreate event called FormCreate.

unit Unit1;

interface

uses
  Forms, Dialogs;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation
{$R *.dfm} // Include form definitions

// A small procedure
procedure InLineProc;
begin
  ShowMessage('Hello World');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Call our little in line procedure
  InLineProc;
end;

end.
```

The `InLineProc` we have defined above is literally that - an in-line subroutine. It must be defined before it is called.

The `TForm1.OnCreate` procedure is quite different. The `TForm1` qualifier gives a clue. This procedure, along with out InLineProc procedure, is defined in what is called the Implementation section of the Unit. Looking earlier in the code, you will see a one line declaration of OnCreate in the Interface part of the Unit. It is part of the class definition for the form *(`TForm1`)* that the Unit and program use as the main screen.

---

```pas
procedure DoIt(A : Integer);
begin
  A := A * 2;
  ShowMessageFmt('A in the procedure  = %d',[A]);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  A : Integer;
begin
  A := 22;
  ShowMessageFmt('A in program before call = %d',[A]);
  // Call the procedure
  DoIt(A);
  ShowMessageFmt('A in program now = %d',[A]);
end;
```

**Output**:

```
A in program before call = 22
A in the procedure = 44
A in program now = 22
```

### 3.4.5. Passing data by reference

```pas
procedure DoIt(Var A : Integer);
begin
  A := A * 2;
  ShowMessageFmt('A in the procedure  = %d',[A]);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  A : Integer;
begin
  A := 22;
  ShowMessageFmt('A in program before call = %d',[A]);
  // Call the procedure
  DoIt(A);
  ShowMessageFmt('A in program now = %d',[A]);
end;
```

**Output**:

```
A in program before call = 22
A in the procedure = 44
A in program now = 44
```

### 3.4.6. Output only parameters

```pas
procedure DoIt(Out A : Integer);
begin
  A := 123;
  ShowMessageFmt('A in the procedure  = %d',[A]);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  A : Integer;
begin
  ShowMessage('A before the call is unknown');
  // Call the procedure
  DoIt(A);
  ShowMessageFmt('A in program now = %d',[A]);
end;
```

**Output**:

```
A before the call is unknown
A in the procedure = 123
A in program now = 123
```

### 3.4.7. Constant value parameters

```pas
procedure DoIt(Const A : Integer; Out B : Integer);
begin
  B := A * 2;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  A, B : Integer;
begin
  A := 22;
  // Call the procedure
  DoIt(A, B);
  ShowMessageFmt('B has been set to = %d',[B]);
end;
```

**Output**: 

```
B has been set to 44
```

### 3.4.8. Same routine, different parameters

```pas
procedure DoIt; overload;
begin
  ShowMessage('DoIt with no parameters called');
end;

procedure DoIt(msg : String); overload;
begin
  ShowMessage('DoIt called with parameter : '+msg);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Call the procedure using no parameters
  DoIt;
  // Now call the procedure using one parameter
  DoIt('Hi there');
end;
```
	
**Output**: 

```
DoIt with no parameters called
DoIt called with parameter : Hi There
```

# 4. Exception handling

## 4.1. Try, except

```pas
begin
  Try
    ...
    The code we want to execute
    ...
  Except
    ...
    This code gets executed if an exception occurs in the above block
    ...
  end;
end;
```

```pas
var
  number1, number0 : Integer;
begin
  try
    number0 := 0;
    number1 := 1;
    number1 := number1 div number0;
    ShowMessage('1 / 0 = '+IntToStr(number1));
  except
    on E : Exception do
    begin
      ShowMessage('Exception class name = '+E.ClassName);
      ShowMessage('Exception message = '+E.Message);
    end;
  end;
end;
```

```pas
except
    // IO error
    On E : EInOutError do
      ShowMessage('IO error : '+E.Message);
    // Dibision by zero
    On E : EDivByZero do
      ShowMessage('Div by zero error : '+E.Message);
    // Catch other errors
    else
      ShowMessage('Unknown error');
  end;
```

## 4.2. Raising exceptions

| exception name       | description             |
| -------------------- | ----------------------- |
| Exception            | Base class              |
| EAbort               | Abort without dialog    |
| EAbstractError       | Abstract method error   |
| AssertionFailed      | Assert call failed      |
| EBitsError           | Boolean array error     |
| ECommonCalendarError | Calendar calc error     |
| EDateTimeError       | DateTime calc error     |
| EMonthCalError       | Month calc error        |
| EConversionError     | Raised by Convert       |
| EConvertError        | Object convert error    |
| EDatabaseError       | Database error          |
| EExternal            | Hardware/Windows error  |
| EAccessViolation     | Access violation        |
| EControlC            | User abort occured      |
| EExternalException   | Other Internal error    |
| EIntError            | Integer calc error      |
| EDivByZero           | Integer Divide by zero  |
| EIntOverflow         | Integer overflow        |
| ERangeError          | Out of value range      |
| EMathError           | Floating point error    |
| EInvalidArgument     | Bad argument value      |
| EInvalidOp           | Inappropriate operation |
| EOverflow            | Value too large         |
| EUnderflow           | Value too small         |
| EZeroDivide          | Floating Divide by zero |
| EStackOverflow       | Severe Delphi problem   |
| EHeapException       | Dynamic memory problem  |
| EInvalidPointer      | Bad memory pointer      |
| EOutOfMemory         | Cannot allocate memory  |
| EInOutError          | IO error                |
| EInvalidCast         | Object casting error    |
| EInvalidOperation    | Bad component op        |
| EMenuError           | Menu item error         |
| EOSError             | Operating system error  |
| EParserError         | Parsing error           |
| EPrinter             | Printer error           |
| EPropertyError       | Class property error#   |
| EPropReadOnly        | Invalid property access |
| EPropWriteOnly       | Invalid property access |
| EThread              | Thread error            |
| EVariantError        | Variant problem         |

# 5. Dates and times

## 5.1. TDateTime

Date and time processing depends on the `TDateTime` variable.

It is used to hold a date and time combination. It is also used to hold just date or time values - the time and date value is ignored respectively.

`TDateTime` is defined in the `System` unit. Date constants and routines are defined in `SysUtils` and `DateUtils` units.

```pas
var
  date1, date2, date3 : TDateTime;       // TDateTime variables
begin
  date1 := Yesterday;      // Set to the start of yesterday
  date2 := Date;           // Set to the start of the current day
  date3 := Tomorrow;       // Set to the start of tomorrow
  date4 := Now;            // Set to the current day and time
end;
```

**Output**:

```
date1 is set to something like 12/12/2002 00:00:00
date2 is set to something like 13/12/2002 00:00:00
date3 is set to something like 14/12/2002 00:00:00
date4 is set to something like 13/12/2002 08:15:45
```

## 5.2. Short and long month names

```pas
var
  month : Integer;
begin
  for month := 1 to 12 do    // Display the short and long month names
  begin
    ShowMessage(ShortMonthNames[month]);
    ShowMessage(LongMonthNames[month]);
  end;
end;
```

**Output**:

```
Jan
January
Feb
February
Mar
March
Apr
April
May
May
Jun
June
Jul
July
Aug
August
Sep
September
Oct
October
Nov
November
Dec
December
```

## 5.3. Short and long day names

```pas
var
  day : Integer;
begin
  for day := 1 to 12 do    // Display the short and long day names
  begin
    ShowMessage(ShortDayNames[day]);
    ShowMessage(LongDayNames[day]);
  end;
end;
```

**Output**:
	
```
Sun
Sunday
Mon
Monday
Tue
Tuesday
Wed
Wednesday
Thu
Thursday
Fri
Friday
Sat
Saturday
```

## 5.4. Date and time calculations

```pas
DayOfTheMonth // Gives the day of month index for a TDateTime value
DaysBetween   // Gives the whole number of days between 2 dates
DaysInAMonth  // Gives the number of days in a month
DaysInAYear   // Gives the number of days in a year
DecodeDate    // Extracts the year, month, day values from a TDateTime var.
EncodeDate    // Build a TDateTime value from year, month and day values
IncDay        // Increments a TDateTime variable by + or - number of days
IsLeapYear    // Returns true if a given calendar year is a leap year
MinsPerDay    // Gives the number of minutes in a day 
```

## 5.5. Displaying date and time values

### 5.5.1. Displaying date and time values

```pas
var
  myDate : TDateTime;

begin
  // Set up our TDateTime variable with a full date and time :
  // 09/02/2000 at 05:06:07.008 (.008 milli-seconds)
  myDate := EncodeDateTime(2000, 2, 9, 5, 6, 7, 8);

  // Date only - numeric values with no leading zeroes (except year)
  ShowMessage('              d/m/y = '+
              FormatDateTime('d/m/y', myDate));

  // Date only - numeric values with leading zeroes
  ShowMessage('           dd/mm/yy = '+
              FormatDateTime('dd/mm/yy', myDate));

  // Use short names for the day, month, and add freeform text ('of')
  ShowMessage('  ddd d of mmm yyyy = '+
              FormatDateTime('ddd d of mmm yyyy', myDate));

  // Use long names for the day and month
  ShowMessage('dddd d of mmmm yyyy = '+
              FormatDateTime('dddd d of mmmm yyyy', myDate));

  // Use the ShortDateFormat settings only
  ShowMessage('              ddddd = '+
              FormatDateTime('ddddd', myDate));

  // Use the LongDateFormat settings only
  ShowMessage('             dddddd = '+
              FormatDateTime('dddddd', myDate));

  ShowMessage('');

  // Time only - numeric values with no leading zeroes
  ShowMessage('            h:n:s.z = '+
              FormatDateTime('h:n:s.z', myDate));

  // Time only - numeric values with leading zeroes
  ShowMessage('       hh:nn:ss.zzz = '+
              FormatDateTime('hh:nn:ss.zzz', myDate));

  // Use the ShortTimeFormat settings only
  ShowMessage('                  t = '+FormatDateTime('t', myDate));

  // Use the LongTimeFormat settings only
  ShowMessage('                 tt = '+FormatDateTime('tt', myDate));

  // Use the ShortDateFormat + LongTimeFormat settings
  ShowMessage('                  c = '+FormatDateTime('c', myDate));
end;
```

**Output**:

```
              d/m/y = 9/2/00
          dd/mm/yy = 09/02/00
  ddd d of mmm yyyy = Wed 9 of Feb 2000
dddd d of mmmm yyyy = Wednesday 9 of February 2000
              ddddd = 09/02/2000
            dddddd = 09 February 2000
                  c = 09/02/2000 05:06:07

            h:n:s.z = 5:6:7.008
      hh:nn:ss.zzz = 05:06:07.008
                  t = 05:06
                tt = 05:06:07
                  c = 09/02/2000 05:06:07
```

### 5.5.2. Formatting control variables

```pas
DateSeparator              = /
TimeSeparator              = :
ShortDateFormat            = dd/mm/yyyy
LongDateFormat             = dd mmm yyyy
TimeAMString               = AM
TimePMString               = PM
ShortTimeFormat            = hh:mm
LongTimeFormat             = hh:mm:ss
ShortMonthNames            = Jan Feb ...
LongMonthNames             = January, February ...
ShortDayNames              = Sun, Mon ...
LongDayNames               = Sunday, Monday ...
TwoDigitYearCenturyWindow  = 50 
```

# 6. Files

## 6.1. Accessing files

First, we must get a **handle** for a named file:

```pas
var
  myFile : TextFile;

begin
  AssignFile(myFile, 'Test.txt');
```

Here we are getting a **handle** to a text file, designated by the `TextFile` type (binary files are of type `File`). We ask Delphi to assign a file handle for a file called `'Test.txt'` which will be assumed to be in the current directory *(as given by the GetCurrentDir routine)*.

```pas
`ReWrite` Opens a file as new - discards existing contents if file exists
`Reset` Opens a file for read and write access
`Append` Opens a file for appending to the end (such as a log file)
```

When we have finished, we must close the file:

```pas 	
CloseFile(myFile);
```

## 6.2. Reading and writing to text files

```pas
var
  myFile : TextFile;
  text   : string;

begin
  // Try to open the Test.txt file for writing to
  AssignFile(myFile, 'Test.txt');
  ReWrite(myFile);

  // Write a couple of well known words to this file
  WriteLn(myFile, 'Hello');
  WriteLn(myFile, 'World');

  // Close the file
  CloseFile(myFile);

  // Reopen the file for reading
  Reset(myFile);

  // Display the file contents
  while not Eof(myFile) do
  begin
    ReadLn(myFile, text);
    ShowMessage(text);
  end;

  // Close the file for the last time
  CloseFile(myFile);
end;
```

**Output**:

```
Hello
World
```

If we replaced the `ReWrite` routine with `Append`, and rerun the code, the existing file would then contain:

``` 	
Hello
World
Hello
World
```

```pas
var
  myFile : TextFile;
  text   : string;
  i      : Integer;

begin
  // Try to open the Test.txt file for writing to
  AssignFile(myFile, 'Test.txt');
  ReWrite(myFile);

  // Write a couple of well known words to this file
  Write(myFile, 'Hello ');
  Write(myFile, 'World');

  // Terminate this line
  WriteLn(myFile);

  // Write some numbers to the file as a single line
  for i := 2 to 4 do
    Write(myFile, i/2, '  ');

  // Terminate this line
  WriteLn(myFile);

  // repeat the above, but with number formatting
  for i := 2 to 4 do
    Write(myFile, i/2:5:1);

  // Terminate this line
  WriteLn(myFile);

  // Close the file
  CloseFile(myFile);

  // Reopen the file for reading only
  Reset(myFile);

  // Display the file contents
  while not Eof(myFile) do
  begin
    ReadLn(myFile, text);
    ShowMessage(text);
  end;

  // Close the file for the last time
  CloseFile(myFile);
end;
```
	
**Output**:

```
Hello World
1.00000000000000E+0000  1.50000000000000E+0000  2.00000000000000E+0000
1.0  1.5  2.0
```

### 6.2.1. Example code : Reading one character at a time from a text file line

```pas
var
  myFile : TextFile;
  letter : char;
  text   : string;

begin
  // Try to open the Test.txt file for writing to
  AssignFile(myFile, 'Test.txt');
  ReWrite(myFile);

  // Write lines of text to the file
  WriteLn(myFile, 'Hello');
  WriteLn(myFile, 'To you');

  // Close the file
  CloseFile(myFile);

  // Reopen the file for reading
  Reset(myFile);

  // Display the file contents
  while not Eof(myFile) do
  begin
    // Proces one line at a time
    ShowMessage('Start of a new line :');
    while not Eoln(myFile) do
    begin
      Read(myFile, letter);   // Read and display one letter at a time
      ShowMessage(letter);
    end;
    ReadLn(myFile, text);
  end;

  // Close the file for the last time
  CloseFile(myFile);
end;
```

**Output**:

```
Start of a new line :
H
e
l
l
o
Start of a new line :
T
o

y
o
u
```

## 6.3. Reading and writing to typed binary files

Typed binary files are files that have a data type as the basic unit of writing and reading. You write, say, an Integer, or a Record to a file, and read the same unit of data back. Records are particularly useful, allowing us to store any mix of data types in the one file unit of data.

```pas
type
  TCustomer = Record
    name : string[20];
    age  : Integer;
    male : Boolean;
  end;

var
  myFile   : File of TCustomer;  // A file of customer records
  customer : TCustomer;          // A customer record variable

begin
  // Try to open the Test.cus binary file for writing to
  AssignFile(myFile, 'Test.cus');
  ReWrite(myFile);

  // Write a couple of customer records to the file
  customer.name := 'Fred Bloggs';
  customer.age  := 21;
  customer.male := true;
  Write(myFile, customer);

  customer.name := 'Jane Turner';
  customer.age  := 45;
  customer.male := false;
  Write(myFile, customer);

  // Close the file
  CloseFile(myFile);

  // Reopen the file in read only mode
  FileMode := fmOpenRead;
  Reset(myFile);

  // Display the file contents
  while not Eof(myFile) do
  begin
    Read(myFile, customer);
    if customer.male
    then ShowMessage('Man with name '+customer.name+
                    ' is '+IntToStr(customer.age))
    else ShowMessage('Lady with name '+customer.name+
                    ' is '+IntToStr(customer.age));
  end;

  // Close the file for the last time
  CloseFile(myFile);
end;
```

**Output**:

```
Man with name Fred Bloggs is 21
Lady with name Jane Turner is 45
```

## 6.4. Reading and writing to pure binary files

```pas
var
  myFile    : File;
  byteArray : array[1..8] of byte;
  oneByte   : byte;
  i, count  : Integer;

begin
  // Try to open the Test.byt file for writing to
  AssignFile(myFile, 'Test.byt');
  ReWrite(myFile, 4);   // Define a single 'record' as 4 bytes

  // Fill out the data array
  for i := 1 to 8 do
    byteArray[i] := i;

  // Write the data array to the file
  BlockWrite(myFile, byteArray, 2);   // Write 2 'records' of 4 bytes

  // Fill out the data array with different data
  for i := 1 to 4 do
    byteArray[i] := i*i;              // Value : 1, 4, 9, 16

  // Write only the first 4 items from the data array to the file
  BlockWrite(myFile, byteArray, 1);   // Write 1 record of 4 bytes

  // Close the file
  CloseFile(myFile);

  // Reopen the file for reading only
  FileMode := fmOpenRead;
  Reset(myFile, 1);   // Now we define one record as 1 byte

  // Display the file contents
  // Start with a read of the first 6 bytes. 'count' is set to the
  // actual number read
  ShowMessage('Reading first set of bytes :');
  BlockRead(myFile, byteArray, 6, count);

  // Display the byte values read
  for i := 1 to count do
    ShowMessage(IntToStr(byteArray[i]));

  // Now read one byte at a time to the end of the file
  ShowMessage('Reading remaining bytes :');
  while not Eof(myFile) do
  begin
    BlockRead(myFile, oneByte, 1);   // Read and display one byte at a time
    ShowMessage(IntToStr(oneByte));
  end;

  // Close the file for the last time
  CloseFile(myFile);
end;
```

**Output**:

```
Reading first set of bytes :
1
2
3
4
5
6
Reading remaining bytes :
7
8
1
4
9
16
```

## 6.5. Other file processing mechanisms

| name       | description                                      |
| ---------- | ------------------------------------------------ |
| `FilePos`  | Gives the file position in a binary or text file |
| `Seek`     | Moves to a new position in the file              |
| `SeekEof`  | Skip to the end of the current line or file      |
| `SeekEoln` | Skip to the end of the current line or file      |

## 6.6. Getting information about files and directories

| name              | description                                              |
| ----------------- | -------------------------------------------------------- |
| `ChDir`           | Change the working drive plus path for a specified drive |
| `CreateDir`       | Create a directory                                       |
| `DeleteFile`      | Delete a file specified by its file name                 |
| `Erase`           | Erase a file                                             |
| `FileExists`      | Returns true if the given file exists                    |
| `FileSearch`      | Search for a file in one or more directories             |
| `FileSetDate`     | Set the last modified date and time of a file            |
| `Flush`           | Flushes buffered text file data to the file              |
| `GetCurrentDir`   | Get the current directory (drive plus directory)         |
| `MkDir`           | Make a directory                                         |
| `RemoveDir`       | Remove a directory                                       |
| `Rename`          | Rename a file                                            |
| `RenameFile`      | Rename a file or directory                               |
| `RmDir`           | Remove a directory                                       |
| `SelectDirectory` | Display a dialog to allow user selection of a directory  |
| `SetCurrentDir`   | Change the current directory                             |
| `Truncate`        | Truncates a file size                                    |

## 6.7. Using TStringList to read and write text files

The `TStringList` class is a very useful utility class that works on a lits of strings, each indexable like an array. The list can be sorted, and supports name/value pair strings, allowing selection by name or value.

These lists can be furnished from text files in one fell swoop.

```pas
var
  fileData : TStringList;                // Our TStringList variable
begin
  fileData := TStringList.Create;        // Create the TSTringList object
  fileData.LoadFromFile('Testing.txt');  // Load from Testing.txt file
  ...
```

We can display the whole file in a `Memo box`:

```pas 	
memoBox.Text := fileData.Text;
```

And we can display or process the file with direct access to any line at any time. In the example code below, we open a text file, reverse all lines in the file, and then save it back. Not terribly useful, but it shows the power of TStringList.
 	
```pas	
var
  fileData : TStringList;
  saveLine : String;
  lines, i : Integer;
begin
  fileData := TStringList.Create;        // Create the TSTringList object
  fileData.LoadFromFile('Test.txt');     // Load from Testing.txt file

  // Reverse the sequence of lines in the file
  lines := fileData.Count;

  for i := lines-1 downto (lines div 2) do
  begin
    saveLine := fileData[lines-i-1];
    fileData[lines-i-1] := fileData[i];
    fileData[i] := saveLine;
  end;

  // Now display the file
  for i := 0 to lines-1 do
    ShowMessage(fileData[i]);

  fileData.SaveToFile('Test.txt');       // Save the reverse sequence file
end;
```

## 6.8. Extract file path

The `ExtractFilePath` function extracts from `FullFileName` the path substring.
 
This is the part of the full file name up to and including the final `\` before the file name. 

```pas
var
  fullFileName : string;

begin
  // Set up a full file name with drive and path
  fullFileName := 'C:\Program Files\Borland\Delphi7\Projects\Unit1.dcu';

  // Show the component parts of this full name
  ShowMessage('Drive = '+ExtractFileDrive (fullFileName));
  ShowMessage('Dir   = '+ExtractFileDir   (fullFileName));
  ShowMessage('Path  = '+ExtractFilePath  (fullFileName));
  ShowMessage('Name  = '+ExtractFileName  (fullFileName));
  ShowMessage('Ext   = '+ExtractFileExt   (fullFileName));
end;
```

**Output**:

```
Drive = C:
Dir   = C:\Program Files\Borland\Delphi7\Projects
Path  = C:\Program Files\Borland\Delphi7\Projects\
Name  = Unit1.dcu
Ext   = .dcu
```

## 6.9. ParamStr

The `ParamStr` function returns one of the parameters from the command line used to invoke the current program.
 
The `ParamIndex` parameter determines which parameter is returned:
 
`0` : The execution drive/path/program 1 : Return the 1st parameter 2 : Return the 2nd parameter ...
 
If there is no parameter value for the given index,an empty string is returned. 

```pas
var
  cmd : string;
  i : Integer;

begin
  // Before running this code, use the Run/parameters menu option
  // to set the following command line parameters : -parm1 -parm2

  // Show these parameters - note that the 0th parameter is the
  // execution command on Windows
  for i := 0 to ParamCount do
    ShowMessage('Parameter '+IntToStr(i)+' = '+ParamStr(i));
end;
```

# 7. Conversions

## 7.1. Integer to string

```pas
var
  NormalInteger : Integer;
  BigInteger    : Int64;

begin
  NormalInteger := 2147483647;          // Largest possible Integer value
  BigInteger    := 9223372036854775807; // Largest possible Int64 value

  ShowMessage('NormalInteger :     '+IntToStr(NormalInteger));
  ShowMessage('BigInteger :        '+IntToStr(BigInteger));
  ShowMessage('Calculated number : '+IntToStr(27 * 4));
end;
```

## 7.2. Float to string

```pas
var
  amount1, amount2, amount3 : Extended;
begin
  amount1 := 1234567890.123456789;  // High precision number
  amount2 := 1234567890123456.123;  // High mantissa digits
  amount3 := 1E100;                 // High value number

  ShowMessage('Amount1 = '+FloatToStr(amount1));
  ShowMessage('Amount2 = '+FloatToStr(amount2));
  ShowMessage('Amount3 = '+FloatToStr(amount3));
end;
```

## 7.3. String to integer

```pas
var
  A, B, C, D, E, F : Integer;

begin
  A := 32;
  B := StrToInt('100');    // '100' string converted to 100 integer
  C := StrToInt('  -12');  // Leading blanks are ignored
  D := StrToInt('$1E');    // Hexadecimal values start with a '$'
  E := StrToInt('-0x1E');  // ... or with a '0x'
  F := A + B + C + D + E;  // Lets add up all these integers

  ShowMessage('A : '+IntToStr(A));
  ShowMessage('B : '+IntToStr(B));
  ShowMessage('C : '+IntToStr(C));
  ShowMessage('D : '+IntToStr(D));
  ShowMessage('E : '+IntToStr(E));
  ShowMessage('F : '+IntToStr(F));
end;
```

```pas
var
  A : Integer;

begin
  // We will catch conversion errors
  try
    A := StrToInt('100 ');    // Trailing blanks are not supported
  except
    on Exception : EConvertError do
      ShowMessage(Exception.Message);
  end;

  try
    A := StrToInt('$FG');    // 'G' is an invalid hexadecimal digit
  except
    on Exception : EConvertError do
      ShowMessage(Exception.Message);
  end;
end;
```

## 7.4. Tryparse string to integer

```pas
if not TryStrToInt(StrVal, IntVal) then
begin
// error handling
end;
```

# 8. Format

The Format function provides 'C' like formatting of multiple of simple data types into a string. It provides very precise control over this formatting.
 
The Formatting parameter defines how the Data array is manipulated into the returned string.
 
The Formatting string can comprise a mix of ordinary characters (that are passed unchanged to the result string), and data formatting characters. This formatting is best explained by the example code.
 
In simple terms, each data formatting substring starts with a % and ends with a data type indicator :
 
|     |                   |
| --- | ----------------- |
| d   | Decimal (integer) |
| e   | Scientific        |
| f   | Fixed             |
| g   | General           |
| m   | Money             |
| n   | Number (floating) |
| p   | Pointer           |
| s   | String            |
| u   | Unsigned decimal  |
| x   | Hexadecimal       |

 
The general format of each formatting substring is as follows:
 
%[Index:][-][Width][.Precision]Type
 
where the square brackets refer to optional parameters, and the : . - characters are literals, the first 2 of which are used to identify two of the optional arguments.
 
Version 2 of this function is for use within threads. You furnish the FormatSettings record before invoking the call. It takes a local copy of global formatting variables that make the routine thread safe.

[more](http://www.delphibasics.co.uk/RTL.asp?Name=Format)

# 9. Class

```pas
type
  className = class(BaseClass)
  private
    // Data/method defs local to this Unit
  protected
    // Data/method defs local to this class + descendants
  public
    // Data/method defs usable with all objects of this class
  published
    // Externally interrogatable public definitions
end;
```

Fruit class:

```pas
TFruit = Class(TObject)  // This is an actual class definition :
    // Internal class field definitions - only accessible in this unit
    private
      isRound  : Boolean;
      length   : single;
      width    : single;
      diameter : single;
    // Fields and methods only accessible by this class and descendants
    protected
    // Externally accessible fields and methods
    public
      // 2 constructors - one for round fruit, the other long fruit
      constructor Create(diameter : single);               overload;
      constructor Create(length : single; width : single); overload;
    // Externally accessible and inspectable fields and methods
    published
      // Note that properties must use different names to local defs
      property round : Boolean
        read   isRound;
      property len   : single
        read   length;
      property wide  : single
        read   width;
      property diam  : single
        read   diameter;
  end;                    // End of the TFruit class definition
```

Create constructors:

```pas
// Create a round fruit object
constructor TFruit.Create(diameter: single);
begin
  // Indicate that we have a round fruit, and set its size
  isRound       := true;
  self.diameter := diameter;
end;

// Create a long fruit object
constructor TFruit.Create(length, width: single);
begin
  // Indicate that we have a long fruit, and set its size
  isRound     := false;
  self.length := length;
  self.width  := width;
end;
```

Create instance of `TFruit`:

```pas
var apple, banana : TFruit;

apple  := TFruit.Create(3.5);
banana := TFruit.Create(7.0, 1.75);
```

# 10. UI elements

## 10.1. Label

```pas
Label1.Caption := 'szöveg'
```

## 10.2. Dialog

The `DialogType` may have one of the following enumerated values:
 
| name             | description                   |
| ---------------- | ----------------------------- |
| `mtWarning`      | Displays a exclamation symbol |
| `mtError`        | Displays a red 'X'            |
| `mtInformation`  | Displays an 'i' in a bubble   |
| `mtConfirmation` | Displays an question mark     |
| `mtCustom`       | Displays just the message     |

The `Buttons` value may be one or more of the following enumerated values :

| name         | description                   |
| ------------ | ----------------------------- |
| `mbYes`      | Displays a 'Yes' button       |
| `mbNo`       | Displays a 'No' button        |
| `mbOK`       | Displays an 'OK' button       |
| `mbCancel`   | Displays a 'Cancel' button    |
| `mbAbort`    | Displays an 'Abort' button    |
| `mbRetry`    | Displays a 'Retry' button     |
| `mbIgnore`   | Displays an 'Ignore' button   |
| `mbAll`      | Displays an 'All' button      |
| `mbNoToAll`  | Displays a 'No to all' button |
| `mbYesToAll` | Displys a 'Yes to all' button |
| `mbHelp`     | Displays a 'Help' button      |

You specify these values comma separated in square brackets, as in the second code example.
 
Delphi provides a number of predefined button combinations:
 
| name                  | description                                 |
| --------------------- | ------------------------------------------- |
| `mbYesNoCancel`       | [mbYes,mbNO,mbCancel]                       |
| `mbYesAllNoAllCancel` | [mbYes,mbYesToAll, mbNo,mbNoToAll,mbCancel] |
| `mbOKCancel`          | [mbOK,mbCancel]                             |
| `mbAbortRetryCancel`  | [mbAbort,mbRetry,mbCancel]                  |
| `mbAbortIgnore`       | [mbAbort,mbIgnore]                          |

Now Delphi seem to have made a design error when setting the return value from the dialog box. Instead of specifying the enumeration value of the button pressed, it uses a completely different set of enumeration names:
 
|              |     |
| ------------ | --- |
| `mrYes`      | 6   |
| `mrNo`       | 7   |
| `mrOK`       | 1   |
| `mrCancel`   | 2   |
| `mrAbort`    | 3   |
| `mrRetry`    | 4   |
| `mrIgnore`   | 5   |
| `mrAll`      | 8   |
| `mrNoToAll`  | 9   |
| `mrYesToAll` | 10  |

The values given are the numerical values of these enumerations, given in the numerical order that the mb equivalents are defined. This is very odd.
 
Additionally, these values are defined in the Controls unit, not the Dialogs unit.
 
Note that the Help button has no equivalent return value. This is because it does not terminate the dialog.
 
The HelpContext value is used in conjunction with the Help button. It's use is beyond the scope of Delphi Basics. 

### 10.2.1. Confirmation dialog

```pas
var
  buttonSelected : Integer;
begin
  // Show a confirmation dialog
  buttonSelected := MessageDlg('Confirmation',mtError, mbOKCancel, 0);

  // Show the button type selected
  if buttonSelected = mrOK     then ShowMessage('OK pressed');
  if buttonSelected = mrCancel then ShowMessage('Cancel pressed');
end;
```

### 10.2.2. Custom dialog with custom button selection

```pas
var
  buttonSelected : Integer;
begin
  // Show a custom dialog
  buttonSelected := MessageDlg('Custom dialog',mtCustom,
                              [mbYes,mbAll,mbCancel], 0);

  // Show the button type selected
  if buttonSelected = mrYes    then ShowMessage('Yes pressed');
  if buttonSelected = mrAll    then ShowMessage('All pressed');
  if buttonSelected = mrCancel then ShowMessage('Cancel pressed');
end;
```

## 10.3. Picture

```pas
PlayerCard1.Picture.LoadFromFile('C:\Users\w\Desktop\delphi\black_jack\images\AC.PNG');
```

## 10.4. Timer

```pas
Ora := TTimer.Create(Self);
Ora.Interval := 100; // 100 ms
Ora.OnTimer := LetsGo; // call procedure in every 100 ms
Ora.Enabled := false; // don't start
```

## 10.5. Toolbar

## 10.6. Form

`OnCreate`

### 10.6.1. Create another form

Other form:

```pas
unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    ListBox1: TListBox;
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner:Tcomponent;Valasztek:TStrings);
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

constructor TForm2.Create(AOwner: TComponent; Valasztek:TStrings);
begin
  inherited Create(Owner);
  Listbox1.items.Assign(valasztek);
end;

end.
```

Create the other form

```pas
uses .. , Unit2;

...

procedure TForm1.Button1Click(Sender: TObject);
var otherForm : TForm2;
var items : TStringList;
begin
  // create a list and put items in it
  items := TStringList.Create;
  items.Add('elso');
  items.Add('masodik');
  items.Add('harmadik');

  // call the constructor
  otherForm:=TForm2.Create(Self, items);
  // I don't know what is it but without this, the other form will not open
  if otherForm.ShowModal=mrOk then
  begin

  end;
  items.Free;
  otherForm.free;
end;
```

## 10.7. ListBox

Add item: `ListBox1.Items.Add(string)`

Delete item: `ListBox1.Items.Delete(index)`

Get selected:

```pas
for i := 0 to ListBox1.Items.Count - 1 do
begin
  if ListBox1.Selected[i] then
  begin
      selectedItemIndex := i;
  end;
end;
```

`Multiselect := true` / `false`

## 10.8. Drag and drop

### 10.8.1. ListBox

There is 2 listboxes

Drag items from listbox1 to listbox2

Add items to listbox1

```pas
procedure TForm1.FormCreate(Sender: TObject);
begin
  ListBox1.Items.Add('elso');
  ListBox1.Items.Add('masodik');
  ListBox1.Items.Add('harmadik');
end;
```

If start drag, save the selected items index for later usage

```pas
procedure TForm1.ListBox1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var i : integer;
begin
  for i := 0 to ListBox1.Items.Count - 1 do
  begin
    if ListBox1.Selected[i] then
    begin
        selectedItemIndex := i;
    end;
  end;
end;
```

If there are items in listbox1, enable drag

```pas
procedure TForm1.ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ListBox1.Items.Count > 0 then
  begin
        TListBox(Sender).BeginDrag(false, 5);
  end;
end;
```

Add selected item from listbox1 to listbox2 when dropped on listbox2.

Then delete from listbox1

```pas
procedure TForm1.ListBox2DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  ListBox2.Items.Add(ListBox1.Items[selectedItemIndex]);
  ListBox1.Items.Delete(selectedItemIndex);
end;
```

Enable drag to listbox2

```pas
procedure TForm1.ListBox2DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := true;
end;
```

## 10.9. StringGrid

```pas
StringGrid1.Cells[columnIndex, rowIndex] := 'yo';

// last row last cell
// you will need -1, because of the indexing
StringGrid1.Cells[StringGrid1.ColCount - 1, StringGrid1.RowCount - 1] := 'ez';
```
## Canvas

### Draw line

Draw line on Image1

```pas
with Image1.Canvas do
begin
  Brush.Style := bsClear;
  Pen.Style := psSolid;
  Pen.Color := clGreen;
  Pen.Width := 50;
  MoveTo(19,79);
  LineTo(40,35);
end;
```

### Draw ellipse

```pas
Ellipse(20,200,50,70);
```

# 11. Good to know

## 11.1. Generate random character

'A' to 'Z' = 65 - 90

```pas
chr(random(25) + 65)
```

Or

```pas
uses Math

...

chr(RandomRange(65, 90))
```

## 11.2. Textbox `Edit` when press enter

```pas
procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if ord(Key) = VK_RETURN then
  begin
    Key := #0; // prevent beeping
    WebBrowser1.Navigate(Edit1.Text);
  end;
end; 
```

# 12. Errors

If delphi can't create exe and it isn't opened, find it int Task Manager and kill it.