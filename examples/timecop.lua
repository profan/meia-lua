-- shortform of love.graphics
local lg = love.graphics

require 'fun'()

-- TODOS
-- * use hump's vector light everywhere instead of vector,
--  to avoid creating a new table for every single intermediate calculation,
--  should reduce time spent on GC a lot.
-- * think about system update order, .. it's currently undefined,
--  but order of updating definitely matters.. :D
-- * consider how to structure up having multiple levels and transitioning between them,
--  also how should level local logic be dealt with?
-- * think about how to deal with cooldowns for actions and stuff, shooting delays in weapons, etc.
--   * .. sort of dealt with? not great, but EH
-- * think about how to handle trails for bullets and stuff
-- * incorporate types of projectiles later
-- * show inventory somehow
-- * figure out better way to deplete ammo, ie "points/shot" and -
--  "replenished points/time unit" instead of the current
--  very arbitrary "tweak the numbers until it looks good" method.
-- * figure out state machines of some kind that we can use to describe enemy behaviour,
--    and make sort of interesting npcs hopefully
-- * add counter which keeps track of memory allocated during last second using collectgarbage()
--  and put this in the debug view

-- BUGS
-- * fix bullet removals bug


-- includes
local tiny = require "tiny"
local units_stuff = require "units"
local units, unit_resources = units_stuff[1], units_stuff[2]

-- random utility functions
function clamp(v, min, max)
	return (v < min and min) or (v > max and max) or v
end

function isnan(n, u)
	return n ~= n
end

function point_inside_rect(px, py, x, y, w, h)
	return px < x + w and px > x and py > y and py < y + h
end

-- decides if a thing is within bounds of the level somehow
function is_within_bounds(level, x, y)
	return true
end

-- 2d distance
function distance(v1, v2)
	return (v1 - v2):len()
end

-- types of systems
local updateSystemFilter = tiny.rejectAll("is_drawing_system")
local drawSystemFilter = tiny.requireAll("is_drawing_system")
local hudSystemFilter = tiny.requireAll("is_hud_system")

-- updating logic systems
local physicsSystem = tiny.processingSystem()
physicsSystem.filter = tiny.requireAll("pos", "vel", "mass", "speed")

function physicsSystem:process(e, dt)
	e.pos = e.pos + e.vel
	e.vel = e.vel * (e.speed / e.mass)
end

local childSystem = tiny.processingSystem()
childSystem.filter = tiny.requireAll("parent")

function childSystem:process(e, dt)
	e.pos = e.parent.pos + e.offset
end

local animationSystem = tiny.processingSystem({is_drawing_system = true})
animationSystem.filter = tiny.requireAll("state_id", "dir")

local coordinate_system = Vector(-1, 0)
function animationSystem:process(e, dt)

	-- decide direction depending on direction vector, currently completely wrong but can easily be tweaked once we can see it
	local angle = math.deg(e.dir:angleTo(coordinate_system))

	e.spritesheet.state_id = (((angle > -135 and angle < -45)) and e.spritesheet.states.forward)
		or ((angle > -45 and angle < 135) and angle > -90 and angle < 45) and e.spritesheet.states.left
		or (angle > 0 and angle < 135 and e.spritesheet.states.back)
		or (angle < 270 and e.spritesheet.states.right)
		or e.spritesheet.states.forward

end

local aimingSystem = tiny.processingSystem()

aimingSystem.v = Vector(0, 0)
aimingSystem.filter = tiny.requireAll("pos", "dir", "aim_pos")

function aimingSystem:preWrap(dt)
	local w_x, w_y = self.camera:worldCoords(love.mouse.getPosition())
	self.v.x = w_x
	self.v.y = w_y
end

function aimingSystem:process(e, dt)
	e.dir = (e.pos - self.v):normalizeInplace()
	e.aim_pos = self.v
end

local aimposSystem = tiny.processingSystem()
aimposSystem.filter = tiny.requireAll("parent")
function aimposSystem:process(e, dt)
	e.pos = e.parent.aim_pos
end

-- input system
local inputSystem = tiny.processingSystem()

inputSystem.mouse_wheel = {
	x = 0,
	y = 0
}

inputSystem.keys = {
	w = "forward",
	a = "left",
	s = "back",
	d = "right"
}

inputSystem.mouse = {
	["1"] = "shoot"
}


inputSystem.filter = tiny.requireAny("intents")
function inputSystem:preWrap(dt)
	self.m_x, self.m_y = love.mouse.getPosition()
end

function inputSystem:process(e, dt)

	for key, intent in pairs(self.keys) do
		if love.keyboard.isDown(key) then
			e.intents[intent](e, dt)
		end
	end

	for mkey, intent in pairs(self.mouse) do
		if love.mouse.isDown(mkey) then
			e.intents[intent](e, dt)
		end
	end

	if self.mouse_wheel.y > 0 then
		e.intents.next_item(e, dt)
	elseif self.mouse_wheel.y < 0 then
		e.intents.prev_item(e, dt)
	end

end

function inputSystem:postWrap(dt)

	self.mouse_wheel.x = 0
	self.mouse_wheel.y = 0

end

-- drawing logic systems
local spriteSystem = tiny.processingSystem({is_drawing_system = true})
spriteSystem.filter = tiny.requireAll("spritesheet", "pos")
function spriteSystem:process(e, dt)
	lg.draw(e.spritesheet.image, e.spritesheet.quads[e.spritesheet.state_id], e.pos.x, e.pos.y)
end

local rectangleSystem = tiny.processingSystem({is_drawing_system = true})
rectangleSystem.filter = tiny.requireAll("pos",  "w", "h", "colour")
function rectangleSystem:process(e, dt)

	lg.setColor(e.colour[1], e.colour[2], e.colour[3])
	lg.rectangle("fill", e.pos.x, e.pos.y, e.w, e.h)

end

local weaponSystem = tiny.processingSystem()
weaponSystem.filter = tiny.requireAll("weapons")

function weaponSystem:process(e, dt)

	if e.equipped_weapon == nil and #e.weapons > 0 then
		e.equipped_weapon = e.weapons[1]
		e.equipped_weapon.parent = e
		e.equipped_index = 1
	end

end

local shootingSystem = tiny.processingSystem()

shootingSystem.collidables = {}

shootingSystem.bullets = {
	player = {},
	enemy = {}
}

shootingSystem.removals = {
	player = {i = 0},
	enemy = {i = 0}
}

shootingSystem.filter = tiny.requireAll("parent", "is_shooting", "cooldown")

function shootingSystem:process(e, dt)

	if e.is_shooting then

		local bullet_table = self.bullets[e.parent.owner]

		if e.cooldown > e.shoot_delay and love.timer.getTime() - e.last_shot > e.shoot_delay then

			for i=1, e.num_projectiles do

				local r = (math.random() - 0.5) * e.spread
				local s_dir = e.parent.dir:rotated(r)
				local px, py = e.parent.pos.x, e.parent.pos.y
				local s_dx, s_dy = s_dir.x, s_dir.y

				bullet_table[#bullet_table+1] = {x = px, y = py, dx = s_dx, dy = s_dy}

			end

			e.last_shot = love.timer.getTime()
			e.cooldown = clamp(e.cooldown - (e.continuous_fire * dt), -25, 100)

		end

		e.is_shooting = false

	else
		e.cooldown = clamp(e.cooldown + (e.continuous_fire * dt) / 4, -25, 100)
	end

end

function shootingSystem:removeBullet(owner, i)

	local r_i = self.removals[owner].i + 1
	self.removals[owner].i = r_i
	self.removals[owner][r_i] = i

end

function shootingSystem:postWrap(dt)

	for owner, bullets in pairs(self.bullets) do
		for i=1, #bullets do

			local b = bullets[i]
			-- cornercase, sometimes explodes here, i was 9, #bullets was 10
			b.x, b.y = b.x - b.dx * 4, b.y - b.dy * 4

			-- temporary test :D FIXME...
			if not point_inside_rect(b.x, b.y, 0, 0, 640, 480) then
				local s_i = self.removals[owner].i + 1
				self.removals[owner].i = s_i
				self.removals[owner][s_i] = i
			end

		end
	end

	for owner, removals in pairs(self.removals) do

		local bullets = self.bullets[owner]

		for i=1, removals.i do
			local b_i = removals[i]
			if bullets[#bullets] ~= bullets[b_i] then
				bullets[#bullets], bullets[b_i] = bullets[b_i], bullets[#bullets]
				bullets[#bullets] = nil
			end
		end

		removals.i = 0

	end

end

local collisionSystem = tiny.processingSystem()

collisionSystem.filter = tiny.requireAll("owner", "pos", "vel", "w", "h")

function collisionSystem:process(e, dt)

	local enemy_bullets = (e.owner == "player" and "enemy") or "player"

	-- test for bullet hits here.. because of reasons OK?
	for i=1, #self.bullets[enemy_bullets] do

		local b = self.bullets[enemy_bullets][i]
		if point_inside_rect(b.x, b.y, e.pos.x, e.pos.y, e.w, e.h) then
			e.health = e.health - 10
			e.vel = e.vel + ((e.pos + Vector(e.w/2, e.h/2)) - Vector(b.x + 4, b.y + 4))
			self.shooting_system:removeBullet(enemy_bullets, i)
		end

	end

end

local deathSystem = tiny.processingSystem()
deathSystem.filter = tiny.requireAll("health")

function deathSystem:process(e, dt)

	if e.health <= 0 then
		self.world:removeEntity(e)
	end

end

function is_enemy(e) return e.owner == "enemy" end
function is_player(e) return e.owner == "player" end

local followingSystem = tiny.processingSystem()

function followingSystem:filter(e)
	return (e.pos and e.dir and e.vel and e.speed and e.max_speed and is_enemy(e))
		or (e.pos and e.health and is_player(e))
end

function followingSystem:preWrap(dt)

	-- collect all potential targets, in this case, everyone who isn't an "enemy"
	self.targets = filter(function(e) return is_player(e) end, self.entities)

end

function followingSystem:process(e, dt)

	if is_enemy(e) then

		local closest_enemy = reduce(function(cur, enemy)

			local new_dist = distance(e.pos, enemy.pos)
			if new_dist < cur.dist then
				cur.enemy = enemy
			end

			return cur

		end, {enemy = nil, dist = math.huge}, self.targets)

		local enemy = closest_enemy.enemy

		-- set velocity, but clamp to max_speed
		e.vel = e.vel + e.dir * dt * e.speed
		local clamped_vel = clamp(e.vel:len(), 0.1, e.max_speed)
		e.vel:trimInplace(clamped_vel)

		e.dir = (enemy.pos - e.pos):normalizeInplace()

	end

end

function followingSystem:postWrap(dt)

end

local pickupSystem = tiny.processingSystem()

pickupSystem.pickup_distance = 16
pickupSystem.filter = tiny.requireAny(
	tiny.requireAll("pos", "weapons"),
	tiny.requireAll("pos", "pickup_type", "pickup_item")
)

function pickupSystem:process(e, dt)

	if not e.weapons then return end

	for i, pickup in ipairs(self.entities) do
		if pickup.pickup_type and not pickup.picked_up then -- if actually a pickup

			if distance(e.pos, pickup.pos) < self.pickup_distance then
				pickup.picked_up = true
				if pickup.pickup_type == "weapon" then
					e.weapons[#e.weapons+1] = pickup.pickup_item
				else
					print("unknown pickup, can't pick up!")
				end
			end

		end
	end

end

function pickupSystem:postWrap(dt)

	for i, pickup in ipairs(self.entities) do
		if pickup.picked_up then
			self.world:removeEntity(pickup)
		end
	end

end

local hudSystem = tiny.processingSystem({is_hud_system = true})
hudSystem.filter = tiny.requireAll("hud_elements")
function hudSystem:process(e, dt)

	local x = e.hud_elements.x
	local y = e.hud_elements.y

	local max_width = 100
	local box_padding = 16
	local total_height = reduce(function(cur, el)
		return (el.cond(e) and cur + el.height) or cur
	end, 0, e.hud_elements)

	-- arbitrary numbers, for... grey with alpha
	lg.setColor(45, 45, 45, 185)

	-- ah, i'm an idiot
	lg.rectangle("fill", x - box_padding, y - box_padding, max_width + box_padding*2, total_height + box_padding * 2)

	for i, element in ipairs(e.hud_elements) do

		if element.cond(e) then
			if element.t == "bar" then
				lg.setColor(element.colour)
				lg.rectangle("fill", x, y, element.value(e), element.height)
				y = y + element.height
				lg.setColor(255, 255, 255)
			elseif element.t == "text" then
				lg.setColor(element.colour)
				lg.print(element.value(e), x, y)
				y = y + element.height
			else
				print("unknown hud element type!")
			end
		end

	end

end

-- game state constructor
local game = {}

function game:new(resources)

	local new_obj = {}

	-- game resources, images etc
	new_obj.resources = resources

	-- set up resources for characters
	set_up_resources(unit_resources)

	-- game camera
	self.last_x = 0
	self.last_y = 0
	self.camera = Camera(0, 0)
	self.camera.smoother = Camera.smooth.damped(10)

	-- current game state
	self.bullet_batch = lg.newSpriteBatch(unit_resources.bullet.image, 5000)

	local w, h = unit_resources.bullet.image:getDimensions()
	self.bullet_quad = lg.newQuad(0, 0, w, h, w, h)
	self.bullets = shootingSystem.bullets

	-- set up aimingsystem with camera
	aimingSystem.camera = self.camera

	-- set up collisionsystem with bullets
	collisionSystem.bullets = shootingSystem.bullets
	collisionSystem.removals = shootingSystem.removals
	collisionSystem.shooting_system = shootingSystem

	new_obj.world = tiny.world(
		physicsSystem,
		animationSystem,
		inputSystem,
		spriteSystem,
		rectangleSystem,
		childSystem,
		aimingSystem,
		aimposSystem,
		shootingSystem,
		collisionSystem,
		weaponSystem,
		pickupSystem,
		hudSystem,
		deathSystem,
		followingSystem
	)

	-- add player unit, his crosshair and his starting weapon
	local player = units.timecop(128, 128)
	local crosshair = units.crosshair()
	crosshair.parent = player

	local blaster = units.blaster()
	local blaster_pickup = units.weapon_pickup(172, 172, blaster)
	blaster.parent = blaster_pickup

	local shotgun = units.shotgun()
	local shotgun_pickup = units.weapon_pickup(128, 172, shotgun)
	shotgun.parent = shotgun_pickup

	new_obj.player_unit = player
	new_obj.world:add(player)
	new_obj.world:add(crosshair)

	new_obj.world:add(blaster)
	new_obj.world:add(blaster_pickup)

	new_obj.world:add(shotgun)
	new_obj.world:add(shotgun_pickup)

	-- add 25 of them for testing
	for i=1, 25 do

		local r_x, r_y = math.random(128, 256), math.random(128, 256)

		-- add test enemy with blaster
		local new_quiff = units.quiff(r_x, r_y)
		new_obj.world:add(new_quiff)

		local quiff_blaster = units.blaster()
		new_quiff.equipped_weapon = quiff_blaster

	end

	setmetatable(new_obj, self)
	self.__index = self

	return new_obj

end

-- state related functions below

function game:init()

	local tile_size = 16

	function make_quads(types, spritesheet)
		local quads = {}
		for i = 1, #types do
			quads[#quads+1] = lg.newQuad((i - 1) * tile_size, 0, tile_size, tile_size, spritesheet:getDimensions())
		end
		return quads
	end

	-- tilemap setup

	local tile_num = 2

	local tile_spritesheet = {
		image = self.resources.tiles.image,
		quads = make_quads(self.resources.tiles.types, self.resources.tiles.image)
	}

	local tile_data = require "level/test"
	self.map = Tilemap:new(tile_spritesheet, tile_data)

end

function game:enter(from)

end

function game:update(dt)

	local aim_vec = self.player_unit.aim_pos - self.player_unit.pos
	local vec_len = aim_vec:len()

	local clamped = clamp(vec_len, 1, 250)
	local adj_vec = aim_vec / (vec_len / clamped)

	local w_x, w_y = self.player_unit.pos.x, self.player_unit.pos.y
	local n_x, n_y = self.player_unit.pos.x + adj_vec.x, self.player_unit.pos.y + adj_vec.y
	n_x, n_y = (not isnan(n_x) and n_x) or 1, (not isnan(n_y) and n_y) or 1
	self.camera:lockPosition(n_x, n_y)
	self.camera:lockPosition(self.player_unit.pos.x, self.player_unit.pos.y)

	self.world:update(dt, updateSystemFilter)

end

function game:draw()

	self.camera:attach()

	-- draw map
	self.map:draw()

	-- draw bullets
	self.bullet_batch:clear()
	for owner, bullets in pairs(self.bullets) do
		for i=1, #bullets do
			local b = bullets[i]
			self.bullet_batch:add(self.bullet_quad, b.x, b.y)
		end
	end

	lg.draw(self.bullet_batch)

	-- draw all else
	local dt = love.timer.getDelta()
	self.world:update(dt, drawSystemFilter)

	self.camera:detach()

	self.world:update(dt, hudSystemFilter)

end

function game:focus(has_focus)

end

function game:keypressed(key, scancode, isrepeat)

end

function game:mousemoved(x, y, dx, dy, istouch)

end

function game:wheelmoved(x, y)

	inputSystem.mouse_wheel.x = x
	inputSystem.mouse_wheel.y = y

end

function game:mousepressed(x, y, btn, istouch)

end

function game:quit()

end

local function some_shite()
end

return game
