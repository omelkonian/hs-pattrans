cp docs/index0.html docs/index.html
fs=`find docs/out/ -name "*.png" | sort | sed -e 's/\(.*\)/"\1"/' | paste -sd ","`
sed -i -f /dev/stdin docs/index.html << EOF
s|%%%|${fs}|g
EOF

