BEGIN{
    split(ARGV[1],arr, "-");
    if (arr[2] == "") {
	while ((getline line < ARGV[2]) > 0) {
	    if (line ~ arr[1]) {
		if (line ~ /\[-[0-9.]+\]/) {
		    match(line, /[0-9.]+/);
		    print arr[1], substr(line, RSTART, RLENGTH);
		}
	    }
	}
    }else{
	print arr[1],arr[2];
    }
}
