#!/bin/bash

git config user.name "GitHub Actions"
git config user.email "actions@users.noreply.github.com"
git checkout outdated || git checkout -b outdated

if ! mix aws_rds_ca_store.certdata --check-outdated; then
  mix aws_rds_ca_store.certdata
  git add .
  git commit -m "Update certificates"
  git push --set-upstream origin outdated

  if [[ $(gh pr list --state open --label "outdated check") == "" ]]; then
    gh pr create --fill --label "outdated check"
  fi
fi
