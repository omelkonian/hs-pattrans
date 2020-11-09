curVersion=$(grep "^version" hs-pattrans.cabal | cut -d: -f2 | tr -d "[:blank:]")
prevVersion=$(git show HEAD^:hs-pattrans.cabal | grep "^version" | cut -d: -f2 | tr -d "[:blank:]")
if [[ $curVersion != $prevVersion ]]; then
  echo "Version has changed, publishing to Hackage."
  cabal new-sdist
  cabal upload -u $HACKAGE_USERNAME -p $HACKAGE_PASSWORD \
    --publish dist-newstyle/sdist/hs-pattrans-$curVersion.tar.gz
  cabal upload -u $HACKAGE_USERNAME -p $HACKAGE_PASSWORD \
    --publish -d dist-newstyle/hs-pattrans-$curVersion-docs.tar.gz
else
  echo "Version has not changed, not publishing to Hackage."
fi
