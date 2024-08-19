const Net = (function () {
    var the_offset = 0;

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

        const req = new Request("/a1/poll?at=" + the_offset, {
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
    const node_id = (Math.random() + 1).toString(36).substring(7);
    var items = {};

    function clock (fetch) {
        fetch.then((res) => {
            const items = res.json();
            const ts = items.map((item) => item.time).reduce(String.max);
            Lc.recv(ts);
        });
    }

    function put(row, col, v, type) {
        const time = Lc.send() + "-" + node_id;
        const item = {row: row, col: col, text: v, time: Lc.send(), type: type};
        items.k = item;
        Net.send(item);
    }

    function put_text(row, col, v) {
        put(row, col, v, "text");
    }

    function vote(row, col, v) {
        put(row, col, v, "vote");
    }

    return {
        put_text: put_text,
        vote: vote
    };
})();

const App = (function () {
    function recv(item) {
        switch(item.type) {
        case "text":
            // update cell
            break;
        case "vote":
            // update overlay
            break;
        case "voting":
            //show or hide overlay
            break;
        case "score":
            //change the color
        }
    };
})();
