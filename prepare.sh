alias toString="sed -e 's/\(.*\)/\"\1\"/'"
alias putCommas="paste -sd ','"

# Fill in results in `index.html`
sed "s|%%%|$(ls docs/out | toString | putCommas)|g" docs/index0.html > docs/index.html

# Fill in images for each result
for r in $(ls docs/out); do
  # Must use `here document` syntax when argument list is too long for sed
  sed -f /dev/stdin docs/template.html > docs/$r.html << EOF
s|%%%|$(find docs/out/$r -name "*.png" | sort | toString | putCommas)|g
EOF
done



