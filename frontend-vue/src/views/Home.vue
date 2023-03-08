<script setup>
import Button from "../components/Button.vue"
</script>

<template>
	<div class="container">
		<div class="glass p-4 center-window">
			<div class="col">
				<div class="form">
					<div class="mb-5">
						<div class="h2 text-center"> Funding </div>
					</div>
					<div class="text-center mb-4">
						<a href="/fund" class="btn btn-primary" style="width: 150px">Fund</a>
					</div>
					<div class="text-center">
						<a href="/collect" class="btn btn-primary" style="width: 150px">Collect Fund</a>
					</div>
				</div>
			</div>
		</div>
	</div>
</template>

<script>
export default {
	name: "Home",
	Components: {
		Button
	}
}
</script>

<style scoped>
</style>
