function(keys, values, rereduce) {
    var largest = "";
    for each (var value in values) {
	if (value > largest) largest = value;
    }
    return largest;
}
