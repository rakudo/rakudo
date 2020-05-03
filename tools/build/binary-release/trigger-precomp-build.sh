#!/usr/bin/env sh

if [ $# -lt 4 ]; then
    echo 'You need to pass:'
    echo '   - version of the release, e.g. "2020.01"'
    echo '   - build revision, typically "01"'
    echo '   - URL of a release .tgz file'
    echo '   - a CircleCI token'
    exit 1
fi

curl \
-u $4: \
-X POST \
-H 'Content-Type: application/json' \
-d "{
  \"parameters\": {
    \"BUILD_PRECOMP_RELEASE\": true,
    \"RELEASE_URL\": \"$3\",
    \"VERSION\": \"$1\",
    \"BUILD_REV\": \"$2\"
  }
}" \
https://circleci.com/api/v2/project/gh/rakudo/rakudo/pipeline

