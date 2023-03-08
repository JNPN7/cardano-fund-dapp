import { createRouter, createWebHistory } from 'vue-router'
import Home from '../views/Home.vue'
import Fund from '../views/Fund.vue'
import Collect from '../views/Collect.vue'


const routes = [
  {
    path: '/',
    name: 'Home',
    component: Home,
  },
  {
    path: '/fund',
    name: 'Fund',
    component: Fund,
  },
  {
    path: '/collect',
    name: 'Collect',
    component: Collect,
  },

]

console.log(import.meta.env.BASE_URL)

const router = createRouter({
  history: createWebHistory(import.meta.env.BASE_URL),
  routes,
})

export default router
