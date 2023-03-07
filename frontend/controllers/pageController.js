const home = (req, res) => {
    res.render('home', {title: 'Home'});
}

const fund = (req, res) => {
    res.render('fund', {title: 'Fund'});
}

const collect = (req, res) => {
    res.render('collect', {title: 'Collect'});
}

module.exports = {
	home,
	fund,
	collect
}
