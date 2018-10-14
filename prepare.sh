cp docs/charts0.html docs/charts.html
fs=`find docs/out/ -name "*.png" | sort | sed -e 's/\(.*\)/"\1"/' | paste -sd ","`
sed -i -f /dev/stdin docs/charts.html << EOF
s|%%%|${fs}|g
EOF

