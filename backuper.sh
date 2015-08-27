if [[ "$1" == "h" ]]; then
	rm ~/digital_library/backups/twoHour -rf
	mv ~/digital_library/backups/oneHour ~/digital_library/backups/twoHour
	mongodump -d digital_library -o ~/digital_library/backups/oneHour
fi

if [[ "$1" == "d" ]]; then
	rm ~/digital_library/backups/twoDay -rf
	mv ~/digital_library/backups/oneDay ~/digital_library/backups/twoDay
	mongodump -d digital_library -o ~/digital_library/backups/oneDay
fi

if [[ "$1" == "w" ]]; then
	rm ~/digital_library/backups/twoWeek -rf
	mv ~/digital_library/backups/oneWeek ~/digital_library/backups/twoWeek
	mongodump -d digital_library -o ~/digital_library/backups/oneWeek
fi