function(keys, values, rereduce) {
    var largest = "";
    for (var i=0;i<values.length;i++) {
	if (values[i] > largest) largest = values[i]
    }
    return largest;
}
