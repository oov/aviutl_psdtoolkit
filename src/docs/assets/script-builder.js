function BuilderBase(form, parameterKeys) {
    this.form = form;
    this.parameterKeys = parameterKeys;
    this.latest = {};

    var that = this;
    var changed = function () {
        that.update();
    };
    for (var i = 0; i < parameterKeys.length; ++i) {
        form[parameterKeys[i]].addEventListener('change', changed);
    }

    form.addEventListener('submit', function (e) {
        e.preventDefault();
    });
    form.output.addEventListener('focus', function () {
        form.output.select();
    });
    form.copy.addEventListener('click', function () {
        form.output.select();
        document.execCommand('copy');
    });
}

BuilderBase.prototype.get = function (key, noUpdateState) {
    var value;
    switch (this.form[key].type) {
        case 'text':
            value = this.form[key].value;
            break;
        case 'checkbox':
            value = this.form[key].checked;
            break;
        default:
            throw new Error('unexpected type value: ' + this.form[key].type);
    }
    if (!noUpdateState) {
        this.latest[key] = value;
    }
    return value;
};

BuilderBase.prototype.set = function (key, value, noUpdateState) {
    switch (this.form[key].type) {
        case 'text':
            this.form[key].value = value;
            break;
        case 'checkbox':
            this.form[key].checked = value;
            break;
        default:
            throw new Error('unexpected type value: ' + this.form[key].type);
    }
    if (!noUpdateState) {
        this.latest[key] = value;
    }
};

BuilderBase.prototype.isModified = function (key) {
    return this.latest[key] != this.get(key, true);
}

BuilderBase.prototype.update = function () {
    var modified = false;
    var keys = this.parameterKeys;
    for (var i = 0; i < keys.length; ++i) {
        if (this.isModified(keys[i])) {
            modified = true;
            break;
        }
    }
    if (modified) {
        this.set('output', this.getOutput());
    }
};

function BlinkerBuilder(form) {
    BuilderBase.call(this, form, ['m0', 'm1', 'm2', 'm3', 'm4', 'interval', 'speed', 'offset']);
}
BlinkerBuilder.prototype = Object.create(BuilderBase.prototype);
BlinkerBuilder.prototype.constructor = BlinkerBuilder;
BlinkerBuilder.prototype.getOutput = function () {
    var patterns = [
        this.get('m0'),
        this.get('m1'),
        this.get('m2'),
        this.get('m3'),
        this.get('m4')
    ].filter(function (v) {
        return v != '';
    });
    if (!patterns.length) {
        return '';
    }
    var interval = this.get('interval');
    var speed = this.get('speed');
    var offset = this.get('offset');
    return 'require("PSDToolKit").Blinker.new({"' + patterns.join('","') + '"},' + interval + ',' + speed + ',' + offset + ')';
};

function LipSyncSimpleBuilder(form) {
    BuilderBase.call(this, form, ['m0', 'm1', 'm2', 'm3', 'm4', 'speed', 'layerindex', 'alwaysapply']);
}
LipSyncSimpleBuilder.prototype = Object.create(BuilderBase.prototype);
LipSyncSimpleBuilder.prototype.constructor = LipSyncSimpleBuilder;
LipSyncSimpleBuilder.prototype.getOutput = function () {
    var patterns = [
        this.get('m0'),
        this.get('m1'),
        this.get('m2'),
        this.get('m3'),
        this.get('m4')
    ].filter(function (v) {
        return v != '';
    });
    if (!patterns.length) {
        return '';
    }
    var speed = this.get('speed');
    var layerindex = this.get('layerindex');
    var alwaysapply = this.get('alwaysapply') ? 'true' : 'false';
    return 'require("PSDToolKit").LipSyncSimple.new({"' + patterns.join('","') + '"},' + speed + ',' + layerindex + ',' + alwaysapply + ')';
};

function LipSyncLabBuilder(form) {
    BuilderBase.call(this, form, ['a', 'i', 'u', 'e', 'o', 'N', 'mode', 'layerindex', 'alwaysapply']);
}
LipSyncLabBuilder.prototype = Object.create(BuilderBase.prototype);
LipSyncLabBuilder.prototype.constructor = LipSyncLabBuilder;
LipSyncLabBuilder.prototype.getOutput = function () {
    var a = this.get('a');
    var i = this.get('i');
    var u = this.get('u');
    var e = this.get('e');
    var o = this.get('o');
    var N = this.get('N');
    if (a == '' || i == '' || u == '' || e == '' || o == '' || N == '') {
        return '';
    }
    var mode = this.get("mode");
    var layerindex = this.get("layerindex");
    var alwaysapply = this.get("alwaysapply") ? 'true' : 'false';
    return 'require("PSDToolKit").LipSyncLab.new({a="' + a + '",i="' + i + '",u="' + u + '",e="' + e + '",o="' + o + '",N="' + N + '"},' + mode + ',' + layerindex + ',' + alwaysapply + ')';
};

var blinkerBuilder = null;
var lipsyncSimpleBuilder = null;
var lipsyncLabBuilder = null;

function loaded() {
    var form = document.getElementById('blinker-builder');
    if (form) {
        blinkerBuilder = new BlinkerBuilder(form);
    }
    form = document.getElementById('lipsyncsimple-builder');
    if (form) {
        lipsyncSimpleBuilder = new LipSyncSimpleBuilder(form);
    }
    form = document.getElementById('lipsynclab-builder');
    if (form) {
        lipsyncLabBuilder = new LipSyncLabBuilder(form);
    }
}
document.addEventListener('DOMContentLoaded', loaded);