const Network = (function () {
    var the_offset = 0;
    var the_socket = null;

    function response (res) {
        if (res.status == 200) {
            return res;
        } else {
            throw new Error("oof");
        }
    }

    function sock () {
        const ws = new WebSocket("/a1/socket");
        the_socket = ws;
        return ws;
    }

    function poll () {
        const hs = new Headers();
        hs.append("content-type", "application/json");

        const url = "/a1/poll" + location.search + "&at=" + the_offset;
        const req = new Request(url, {
            method: "GET",
        });

        return req().then(response);
    }

    function send_post (item) {
        const hs = new Headers();
        hs.append("content-type", "application/json");

        const url = "/a1/send" + location.search;
        const req = new Request(url, {
            method: "POST",
            body: item
        });

        return req().then(response);
    }

    function send(item) {
        if (the_socket != null) {
            the_socket.send(item);
        } else {
            send_post(item);
        }
    }

    return {
        poll: poll,
        send: send,
        sock: sock
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

const LamportClock = (function () {
    var the_clock = 0;

    const width = 14;
    const padding = (new Array(width)).fill("0").join("");
    const the_node = (padding + Math.random().toString(16).slice(2)).slice(width);

    function of_string(str) {
        const t = String.split(str, "-", 1);
        return parseInt(t, 10);
    }

    function to_string(ts, node_id = the_node, width = 14) {
        // Number.MAX_SAFE_INTEGER is 14 hex chars. This won't work for negative numbers
        const padding = Array.new(width).fill("0").join("");
        const tss = (padding + (Number(ts).toString(16))).slice(-14);
        return tss + '-' + node_id;
    }

    return {
        node_id: the_node,
        send: () => {
            the_clock = the_clock + 1;
        },
        recv: (ts) => {
            const next = Math.max(the_clock, of_string(ts)) + 1;
            the_clock = next;
            return next;
        }
    };
})();

const AntiEntropy = (function (Lc, Net) {
    const node_id = (Math.random() + 1).toString(36).substring(7);
    var the_items = {};
    var the_recv = null;

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
        the_items.k = item;
        Net.send(item);
        if (the_recv) the_recv(item);
    }

    function put_text(row, col, v) {
        put(row, col, v, "text");
    }

    function vote(row, col, v) {
        put(row, col, v, "vote");
    }

    return {
        put_text: put_text,
        vote: vote,
        set_recv: (recv) => {the_recv = recv;}
    };
})(LamportClock, Network);

const App = (function () {
    function recv(item) {
        switch(item.type) {
        case "text":
            const tr = document.querySelectorAll("table tr")[item.row];
            const td = tr.querySelectorAll("td")[item.col];
            td.innerText = item.text;
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
    }

    function add_option (ev) {
        const tr = document.querySelector("table tbody tr");
        const td = document.createElement("td");
        const col = tr.childElementCount - 1;
        td.id = "0-" + col;
        td.addEventListener("click", text);
        ev.target.before(td);
        Table.add_col(col);
        return false;
    }

    function add_goal (ev) {
        const tb = document.querySelector("table tbody");
        const tr = document.createElement("tr");
        const th = document.createElement("th");
        const row = tb.childElementCount - 1;
        th.id = `${row}-0`;
        th.addEventListener("click", text);
        tr.append(th);
        ev.target.parentElement.before(tr);
        Table.add_row(row);
        return false;
    }

    function text(ev) {
        const id = ev.target.id.split("-", 2);
        const row = parseInt(id[0], 10);
        const col = parseInt(id[1], 10);

        const inp = document.createElement("input");
        inp.type = "text";
        inp.value = ev.target.innerText;
        inp.addEventListener("change", (ev) => {
            AntiEntropy.put_text(row, col, ev.target.value);
            return false;
        });
        ev.target.replaceChildren(inp);
        return false;
    }

    function init() {
        Network.sock();
        AntiEntropy.set_recv(recv);
        Table.init();
    };

    addEventListener("load", init);

    return {
        text: text
    };
})();
