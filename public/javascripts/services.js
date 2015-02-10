var sellers = {
    all: [],

    register: function (url) {
        this.all.push(url);
    },

    sendRequest: function (callBack) {
    	this.all.map(callBack);
    }
};

module.exports.sellers = sellers;