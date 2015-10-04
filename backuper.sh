if [[ "$1" == "h" ]]; then
	rm /var/backups/digital_library/twoHour -rf
	mv /var/backups/digital_library/oneHour /var/backups/digital_library/twoHour
	mongodump -d digital_library -o /var/backups/digital_library/oneHour
fi

if [[ "$1" == "d" ]]; then
	rm /var/backups/digital_library/twoDay -rf
	mv /var/backups/digital_library/oneDay /var/backups/digital_library/twoDay
	mongodump -d digital_library -o /var/backups/digital_library/oneDay
fi

if [[ "$1" == "w" ]]; then
	rm /var/backups/digital_library/twoWeek -rf
	mv /var/backups/digital_library/oneWeek /var/backups/digital_library/twoWeek
	mongodump -d digital_library -o /var/backups/digital_library/oneWeek
fi