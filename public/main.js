const net = (function () {
    function response (res) {
        if (res.status == 200) {
            return res;
        } else {
            throw new Error("oof");
        }
    }

    function poll () {
        const hs = new Headers();
        hs.append("content-type", "application/json");

        const req = new Request("/a1/poll", {
            method: "GET",
        });

        return req().then(response);
    }

    function send (item) {
        const hs = new Headers();
        hs.append("content-type", "application/json");

        const req = new Request("/a1/send", {
            method: "POST",
            body: item
        });

        return req().then(response);
    }

    return {
        poll: poll,
        send: send
    };
})();

const Item = (function () {
    function Item(key, time, data) {
        this.key = key;
        this.time = time;
        this.data = data;
    }

    return Item;
})();

const Lc = (function () {
    var the_clock = 0;

    function time_of(str) {
        const t = String.split(str, "-", 1);
        return parseInt(t, 10);
    }

    return {
        send: () => {
            the_clock = the_clock + 1;
        },
        recv: (ts) => {
            const next = Math.max(the_clock, time_of(ts)) + 1;
            the_clock = next;
            return next;
        }
    };
})();

const Ae = (function () {
    var timestamp = "";
    var items = {};

    function clock (fetch) {
        fetch.then((res) => {
            const items = res.json();
            const ts = items.map((item) => item.time).reduce(String.max);
        });
    }


    return {
        put: put
    };
})();
