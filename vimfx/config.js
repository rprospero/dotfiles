const {classes: Cc, interfaces: Ci, utils: Cu} = Components

let map = (shortcuts, command, custom=false) => {
  vimfx.set(`${custom ? 'custom.' : ''}mode.normal.${command}`, shortcuts)
}

let {Preferences} = Cu.import('resource://gre/modules/Preferences.jsm', {})

vimfx.addCommand({
  name: 'dark',
  description: 'Adjust for low (dark) ambient light'
}, ({vim}) => {
  Preferences.set({
    'devtools.theme': 'dark',
    'extensions.tabtree.theme': 2
  })
  Preferences.reset('extensions.stylish.styleRegistrationEnabled')

})
map(',d', 'dark', true)

vimfx.addCommand({
  name: 'light',
  description: 'Adjust for high ambient light'
}, ({vim}) => {
  Preferences.set('extensions.stylish.styleRegistrationEnabled', false)
  Preferences.reset([
    'devtools.theme',
    'extensions.tabtree.theme'
  ])
})
map(',l', 'light', true)
