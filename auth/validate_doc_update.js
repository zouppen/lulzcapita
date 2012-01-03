function(newDoc, oldDoc, userCtx) {
    if (userCtx.roles.indexOf('_admin') !== -1 ) return;
    if (userCtx.name === "lulzcapita" ) return; // The application.

    throw({forbidden: 'Only admins or the application itself may edit the database'});
}
