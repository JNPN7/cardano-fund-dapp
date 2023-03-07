const express = require('express');
const pageController = require('../controllers/pageController')
const router = express.Router();

router.get('/', pageController.home);
router.get('/fund', pageController.fund);
router.get('/collect', pageController.collect);
module.exports = router;

