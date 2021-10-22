files="ls ./src/Animations"
regex="^(.+)\.hs$"
for f in $($files)
do
    if [[ $f =~ $regex ]]
    then
        name="${BASH_REMATCH[1]}"
        echo "Exporting $name.mp4 from $name.hs"
        stack src/Animations/$name.hs render --target "exports/$name.mp4" --fps 60 --height 1080
    fi
done
