#!/usr/bin/sh

# Correct program verification

output=$(python3 ktsn-entrypoint.py correct.c)
status=$?

if [ $status -ne 0 ]; then
  echo "ERROR: Program exited with status $status"
  exit 1
fi

echo "$output" | grep -q "Successful_verification"
if [ $? -ne 0 ]; then
  echo "ERROR: Output does not contain 'Successful_verification'"
  exit 1
fi

# Bug detection

output=$(python3 ktsn-entrypoint.py invalid_deref.c)
status=$?

if [ $status -ne 0 ]; then
  echo "ERROR: Program exited with status $status"
  exit 1
fi

echo "$output" | grep -q "Invalid_deref"
if [ $? -ne 0 ]; then
  echo "ERROR: Output does not contain 'Invalid_deref'"
  exit 1
fi

echo "All tests passed"
