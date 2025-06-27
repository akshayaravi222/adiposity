grep "impedance" /n/groups/patel/IGLOO/PanUKB/PanUKBshowcase.csv | awk -F',' '{print $6}' | while read -r field; do
    /n/groups/patel/IGLOO/PanUKB/download_sumstats.sh "$field"
done
