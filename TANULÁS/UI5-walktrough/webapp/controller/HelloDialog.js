sap.ui.define([
    "sap/ui/base/ManagedObject",
    "sap/ui/core/Fragment"
], function (ManagedObject, Fragment) {
    "use strict"

    return ManagedObject.extend("sap.ui.demo.walkthrough.controller.HelloDialog", {

        constructor: function (oView) {
            this._oView = oView;
            console.log("HelloDialog constructor called with oView:", oView);
        },

        exit: function () {
            delete this._oView;
            console.log("HelloDialog exit called");
        },

        open: function () {
            var oView = this._oView;
            console.log("HelloDialog open called with oView:", oView);

            // create the dialog lazily
            if (!oView.byId("helloDialog")) {
                var oFragmentController = new ManagedObject({
                    onCloseDialog: function () {
                        console.log("onCloseDialog called");
                        if (oView && oView.byId("helloDialog")) {
                            oView.byId("helloDialog").close();
                        } else {
                            console.error("helloDialog not found in oView");
                        }
                    }
                });
                console.log("Fragment controller created:", oFragmentController);

                // load asynchronous XML fragment
                Fragment.load({
                    id: oView.getId(),
                    name: "sap.ui.demo.walkthrough.view.HelloDialog",
                    controller: oFragmentController
                }).then(function (oDialog) {
                    console.log("Fragment loaded with oDialog:", oDialog);
                    // connect dialog to the root view of the component (models, lifecycle)
                    oView.addDependent(oDialog);
                    oDialog.open();
                }).catch(function (oError) {
                    console.error("Fragment load failed with error:", oError);
                });
            } else {
                oView.byId("helloDialog").open();
            }
        }
    });
});
