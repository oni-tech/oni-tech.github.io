git config --global user.email robots@circleci.com
git config --global user.name CircleCI
cabal exec site rebuild
git checkout master
git pull --rebase
# Overwrite existing files with new files
cp -a _site/. .
#  Commit
git add --all
git commit -m "[`date '+%F %T %Z'`] New release"
# Push
git push origin master:master
