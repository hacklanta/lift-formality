#!/bin/bash

set -ev

sbt ++$TRAVIS_SCALA_VERSION "set liftVersion := \"$LIFT_VERSION\"" test

if grep -q -e "version :=.*SNAPSHOT" build.sbt && [ "${TRAVIS_PULL_REQUEST}" == "false" ] && [ "${TRAVIS_BRANCH}" == "master" ] ; then
  echo "Publishing snapshot to sonatype..."

  openssl aes-256-cbc -K $encrypted_ddfe6500c86a_key -iv $encrypted_ddfe6500c86a_iv -in .sonatype.enc -out ~/.sonatype -d

  sbt ++$TRAVIS_SCALA_VERSION "set liftVersion := \"$LIFT_VERSION\"" publish

  rm ~/.sonatype
fi
