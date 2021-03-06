#!/bin/bash
function tst {
    if [[ "$1" == "$2" ]]
    then
	echo "Success"
    else
	echo "Fail! Got: $A" "Wanted: $2"
    fi
}

# Test not parsed
A=$(echo "giberish" | nc localhost 8000)
tst $A "could-not-parse-request"
# Test create teacher
A=$(echo "add-teacher admin@1234 teacher" | nc localhost 8000)
tst ${A:0:2} "ok"
PSWD=${A:4:4}
# Test id-taken
A=$(echo "add-teacher admin@1234 teacher" | nc localhost 8000)
tst $A "id-taken"
# Test id-taken on student
A=$(echo "add-student admin@1234 teacher" | nc localhost 8000)
tst $A "id-taken"
# Test create student teacher
A=$(echo "add-student teacher@$PSWD studenta" | nc localhost 8000)
tst ${A:0:2} "ok"
PSWD1=${A:4:4}
# Test create student admin
A=$(echo "add-student admin@1234 studentB" | nc localhost 8000)
tst ${A:0:2} "ok"
PSWD2=${A:4:4}
# Test change-pswd student
A=$(echo "change-password studenta@$PSWD1 studentapswd" | nc localhost 8000)
tst ${A:0:2} "ok"
# Test change-pswd teacher
A=$(echo "change-password teacher@$PSWD teacherpswd" | nc localhost 8000)
tst ${A:0:2} "ok"
# Test set-doodle
A=$(echo "set-doodle teacher@teacherpswd Cooking [2016-01-04T14:00+01:00 / 2016-01-04T16:00+0100,2016-01-04T13:00+01:00 / 2016-01-04T15:00+01:00]" | nc localhost 8000)
tst ${A:0:2} "ok"
# Test get-doodle
A=$(echo "get-doodle Cooking" | nc localhost 8000)
tst ${A:0:2} "ok"
# Test subscribe
A=$(echo "subscribe studenta@studentapswd Cooking" | nc localhost 8000)
tst ${A:0:2} "ok"
A=$(echo "subscribe studentB@$PSWD2 Cooking" | nc localhost 8000)
tst ${A:0:2} "ok"
# Test prefer
A=$(echo "prefer studenta@studentapswd Cooking 2016-01-04T14:00+01:00 / 2016-01-04T16:00+0100" | nc localhost 8000)
tst ${A:0:2} "ok"
A=$(echo "prefer studentB@$PSWD2 Cooking 2016-01-04T13:00+01:00 / 2016-01-04T15:00+01:00" | nc localhost 8000)
tst ${A:0:2} "ok"
# Test exam Schedule
# Create some more data
echo "set-doodle teacher@teacherpswd Cleaning [2016-01-05T14:00+01:00 / 2016-01-05T15:00+0100,2016-01-05T13:00+01:00 / 2016-01-05T15:00+01:00]" | nc localhost 8000
echo "set-doodle teacher@teacherpswd Ironing [2016-01-05T14:00+01:00 / 2016-01-05T16:00+0100,2016-01-05T16:00+01:00 / 2016-01-05T17:00+01:00]" | nc localhost 8000
echo "subscribe studentB@$PSWD2 Cleaning" | nc localhost 8000
echo "subscribe studentB@$PSWD2 Ironing" | nc localhost 8000
A=$(echo "exam-schedule studentB@$PSWD2" | nc localhost 8000)
tst ${A:0:2} "ok"
