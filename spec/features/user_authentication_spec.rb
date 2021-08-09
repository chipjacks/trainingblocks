require 'rails_helper'

RSpec.feature 'User authentication', type: :feature do
  let(:user) { FactoryBot.create(:user) }

  xscenario 'User signs in to an existing account' do
    visit new_user_session_path
    fill_in 'user[email]', with: user.email
    fill_in 'user[password]', with: user.password
    click_button 'Log in'
    expect(page).to have_content('Signed in successfully.')
  end

  scenario 'User signs in to a Strava account' do
    visit root_path
    expect_any_instance_of(InitialStravaImportJob).to receive(:perform)
    click_link 'Log in'
    click_link 'Sign in with Strava'
    expect(page).to have_selector('#elm-main')
  end
end
