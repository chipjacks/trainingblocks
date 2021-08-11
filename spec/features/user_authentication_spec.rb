require 'rails_helper'

RSpec.feature 'User authentication', type: :feature do
  let(:user) { FactoryBot.create(:user, :strava) }

  scenario 'User signs in to an existing account' do
    visit new_user_session_path
    fill_in 'user[email]', with: user.email
    fill_in 'user[password]', with: user.password
    click_button 'Log in'
    expect(page).to have_content('Signed in successfully.')
  end

  scenario 'Existing user signs in with a Strava account again' do
    user
    visit new_user_session_path
    click_link 'Sign in with Strava'
    expect(user.auth_token).to be_truthy
    expect(page).to have_selector('#elm-main')
  end

  scenario 'Existing user signs in to a Strava account for first time' do
    user = FactoryBot.create(:user)
    visit new_user_session_path
    click_link 'Sign in with Strava'

    # TODO: Ask user for email and use it to lookup account.
    expect(InitialStravaImportJob).to receive(:perform_now)
    expect(user.auth_token).to be_truthy
    expect(page).to have_selector('#elm-main')
  end
end
