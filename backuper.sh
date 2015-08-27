if [[ "$1" == "h" ]]; then
	rm /var/digital_library/twoHour -rf
	mv /var/digital_library/oneHour /var/digital_library/twoHour
	mongodump -d digital_library -o /var/digital_library/oneHour
fi

if [[ "$1" == "d" ]]; then
	rm /var/digital_library/twoDay -rf
	mv /var/digital_library/oneDay /var/digital_library/twoDay
	mongodump -d digital_library -o /var/digital_library/oneDay
fi

if [[ "$1" == "w" ]]; then
	rm /var/digital_library/twoWeek -rf
	mv /var/digital_library/oneWeek /var/digital_library/twoWeek
	mongodump -d digital_library -o /var/digital_library/oneWeek
fi