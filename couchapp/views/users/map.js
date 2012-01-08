// Emits user nicks and implies IDs.
function(doc) {
    if ( doc.table !== 'user' ) return;
    emit(doc.nick,null);
}
