#!/bin/bash

# USER_PASS=user:password
# SPACE=clojure-contrib

msgfile1="$(tempfile --prefix=msg- --suffix=.txt)"
msgfile2="$(tempfile --prefix=msg- --suffix=.txt)"

# Capture the prompt text from "git commit"
EDITOR="cat > $msgfile1" git commit "$@" 2>/dev/null

# Is there anything to commit?
if cmp -s "$msgfile1" /dev/null; then
  # nope -- and the error has already been printed by git. We're done.
  :
else
  # Commit message prompt text is ready.  Save a copy, and allow the
  # user to edit the original.
  cp "$msgfile1" "$msgfile2"
  $EDITOR "$msgfile1"

  # Did the user edit the commit message
  if cmp -s "$msgfile1" "$msgfile2"; then
    # User did not edit -- cause git to print an appropriate error for
    # an unedited commit message
    EDITOR="cat > /dev/null" git commit "$@"
  else
    # User edited the commit message
    if grep "##" "$msgfile1"; then
      # Found sentinel that will be replaced with the new ticket number
      sum=$(head -n 1 "$msgfile1")

      # Create a new ticket and collect its number
      xml=$(curl -i -X POST -H "Accept: application/xml" -d "ticket[summary]=$sum" "http://$USER_PASS@www.assembla.com/spaces/$SPACE/tickets")

      # TODO: extract ticket number
      sed 's/##/#$ticket/g' -i "$msgfile1"

      # Commit modified message
      git commit --file="$msgfile1" --cleanup="strip" "$@"
    else
      # No ## found in message, so no ticket should be created.
      if grep "#\d" "$msgfile1"; then
        # Some existing ticket was mentioned -- do the commit
        git commit --file="$msgfile1" --cleanup="strip" "$@"
      else
        echo "No ticket or new-ticket sentinel was found.  ^C to abort."
        read
        git commit --file="$msgfile1" --cleanup="strip" "$@"
      fi
    fi
  fi
fi

rm "$msgfile1" "$msgfile2"
